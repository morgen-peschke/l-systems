package com.peschke.fractals.gui

import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.awt.{List => _, _}
import java.util.TimerTask
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}

import com.peschke.fractals.gui.Canvas.Element
import javax.swing._
import javax.swing.border.BevelBorder

class Canvas(defaultDelay: Int, margin: Int = Canvas.DefaultMargin) {
  private final val rawElements = new AtomicReference[Vector[Element]](Vector.empty)
  private final val shiftedElements = new AtomicReference[Vector[Element]](Vector.empty)
  private final val drawnElements = new AtomicReference[Vector[Element]](Vector.empty)
  private final val pendingElements = new AtomicReference[List[Element]](List.empty)
  private final val buffer = new AtomicReference[BufferedImage](Canvas.createImage(500, 500))

  private final val frameDrawnFlag = new AtomicBoolean(false)
  private final val atomicDelay = new AtomicInteger(defaultDelay)
  private final val animatingFlag = new AtomicBoolean(false)
  private final val animationTimer = new java.util.Timer("Canvas Animation Timer")
  private final val atomicFrameAdvanceTask = new AtomicReference[Option[TimerTask]](None)

  private val panel: JPanel = new JPanel() {
    @Override
    override def paintComponent(graphics: Graphics): Unit = {
      super.paintComponent(graphics)
      graphics.create() match {
        case g2D: Graphics2D =>
          val img = buffer.get()
          val clip = g2D.getClipBounds
          val width: Int =
            if (clip.x + clip.width > img.getWidth) img.getWidth - clip.x
            else clip.width

          val height: Int =
            if (clip.y + clip.height > img.getHeight) img.getHeight - clip.y
            else clip.height

          img.getSubimage(clip.x, clip.y, width, height)
          g2D.drawImage(img.getSubimage(clip.x, clip.y, width, height), clip.x, clip.y, this)
          g2D.dispose()
          frameDrawnFlag.set(true)
        case _               => println(s"Not 2D Graphics! <${graphics.getClass.getCanonicalName}>")
      }
    }
  }
  panel.setVisible(true)
  private val scrollPane: JScrollPane = new JScrollPane(panel)
  scrollPane.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED))
  scrollPane.setVisible(true)

  private final def createElementAdvanceTask: TimerTask = new TimerTask {
    override def run(): Unit = {
      if (animatingFlag.get()) {
        if (frameDrawnFlag.get()) {
          frameDrawnFlag.set(false)
          pendingElements.get() match {
            case Nil                         =>
              this.cancel()
              animatingFlag.set(false)
              atomicFrameAdvanceTask.set(None)
            case (f0 @ (shape, color)) :: fx =>
              drawnElements.updateAndGet(_ :+ f0)
              val g = buffer.get().createGraphics()
              g.setColor(color)
              g.draw(shape)
              g.dispose()
              pendingElements.set(fx)
          }
        }
        scrollPane.repaint()
      }
    }
  }

  private def scheduleElementAdvanceTask(): Unit = {
    val frameAdvanceTask = createElementAdvanceTask
    atomicFrameAdvanceTask.getAndSet(Some(frameAdvanceTask)).foreach(_.cancel())
    animationTimer.schedule(
      frameAdvanceTask,
      (atomicDelay.longValue() / 2).max(1L),
      atomicDelay.longValue().max(1L)
    )
  }

  def startAnimation(): Unit = {
    if (!animatingFlag.get()) {
      animatingFlag.set(true)
      drawnElements.set(Vector.empty)
      pendingElements.set(shiftedElements.get.toList)
      scheduleElementAdvanceTask()
    }
    scrollPane.repaint()
  }

  def continueAnimation(): Unit = {
    if (!animatingFlag.get()) {
      animatingFlag.set(true)
      scheduleElementAdvanceTask()
    }
    scrollPane.repaint()
  }

  def component: JComponent = scrollPane

  def appendElements(a: Vector[Element]): Unit =
    SwingUtilities.invokeLater { () =>
      rawElements.updateAndGet(_ ++ a)
      adjustFrames(resetDrawnElements = false)
      continueAnimation()
    }

  def replaceElementsImmediately(a: Vector[Element]): Unit = {
    clear()
    SwingUtilities.invokeLater { () =>
      rawElements.set(a)
      adjustFrames(resetDrawnElements = true)
      val g = buffer.get().createGraphics()
      shiftedElements.get.foreach {
        case (shape, color) =>
          g.setColor(color)
          g.draw(shape)
      }
      g.dispose()
      animatingFlag.set(false)
      panel.repaint()
    }
  }

  def clear(): Unit = SwingUtilities.invokeLater { () =>
    rawElements.set(Vector.empty)
    shiftedElements.set(Vector.empty)
    pendingElements.set(List.empty)
    drawnElements.set(Vector.empty)
    buffer.set(Canvas.createImage(panel.getWidth, panel.getHeight))
    animatingFlag.set(false)
    panel.repaint()
  }

  def updateDrawDelay(delay: Int): Unit = {
    atomicDelay.set(delay.abs)
    if (animatingFlag.get()) {
      scheduleElementAdvanceTask()
    }
  }

  private def adjustFrames(resetDrawnElements: Boolean): Unit = {
    val frames = rawElements.get()
    val (minX, minY) = frames.foldLeft((0d, 0d)) {
      case ((x, y), (shape, _)) =>
        val bounds = shape.getBounds2D
        (x.min(bounds.getMinX), y.min(bounds.getMinY))
    }
    val shift = AffineTransform.getTranslateInstance(-minX, -minY)
    val shifted = frames.map {
      case (shape, color) =>
        val newShape = shift.createTransformedShape(shape)
        (newShape, color)
    }
    shiftedElements.set(shifted)
    val (drawn, pending) = shifted.splitAt(drawnElements.get.length)
    if (resetDrawnElements) {
      drawnElements.set(Vector.empty)
      pendingElements.set(shifted.toList)
    }
    else {
      drawnElements.set(drawn)
      pendingElements.set(pending.toList)
    }
    shifted match {
      case (f0, _) +: fx =>
        val bounds = fx.foldLeft(f0.getBounds2D)(_ createUnion _._1.getBounds2D)
        val width = bounds.getWidth.ceil.toInt.max(1) + margin
        val height = bounds.getHeight.ceil.toInt.max(1) + margin
        panel.setPreferredSize(new Dimension(width, height))
        val newCanvas = Canvas.createImage(width, height)
        buffer.set(newCanvas)
        if (!resetDrawnElements) {
          val g = newCanvas.createGraphics()
          drawn.foreach {
            case (shape, color) =>
              g.setColor(color)
              g.draw(shape)
          }
          g.dispose()
        }
        scrollPane.revalidate()
      case _             => panel.repaint()
    }
  }
}
object Canvas {
  final val DefaultMargin = 100
  type Element = (Shape, Color)

  def createImage(width: Int, height: Int): BufferedImage = new BufferedImage(
    width.max(1),
    height.max(1),
    BufferedImage.TYPE_INT_ARGB
  )
}