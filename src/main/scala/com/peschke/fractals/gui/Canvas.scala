package com.peschke.fractals.gui

import java.awt._
import java.awt.geom.AffineTransform
import java.util.TimerTask
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}

import com.peschke.fractals.gui.Canvas.{Animation, Frame}
import javax.swing._
import javax.swing.border.BevelBorder

class Canvas(defaultDelay: Int, margin: Double = Canvas.DefaultMargin) {
  private final val rawFrames = new AtomicReference[Animation](Vector.empty)
  private final val shiftedFrames = new AtomicReference[Animation](Vector.empty)

  private final val frameDrawnFlag = new AtomicBoolean(false)
  private final val atomicDelay = new AtomicInteger(defaultDelay)
  private final val atomicFrameIndex = new AtomicInteger(0)

  private final val animatingFlag = new AtomicBoolean(false)
  private final val animationTimer = new java.util.Timer("Canvas Animation Timer")
  private final val atomicFrameAdvanceTask = new AtomicReference[Option[TimerTask]](None)

  private val panel: JPanel = new JPanel() {
    @Override
    override def paintComponent(graphics: Graphics): Unit = {
      super.paintComponent(graphics)
      graphics.create() match {
        case g2D: Graphics2D =>
          val frameIndex = atomicFrameIndex.get()
          val framesToDraw = shiftedFrames.get()
          framesToDraw.take(frameIndex).foreach(_.foreach {
            case (shape, color) =>
              g2D.setColor(color)
              g2D.draw(shape)
          })
          g2D.dispose()
        case _               => println(s"Not 2D Graphics! <${graphics.getClass.getCanonicalName}>")
      }
      frameDrawnFlag.set(true)
    }
  }
  panel.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED))
  panel.setVisible(true)
  private val scrollPane: JScrollPane = new JScrollPane(panel)
  scrollPane.setVisible(true)

  private final def createFrameAdvanceTask: TimerTask = new TimerTask {
    override def run(): Unit = {
      if (animatingFlag.get()) {
        if (frameDrawnFlag.get()) {
          val maxIndex = atomicFrameIndex.get()
          val drawActionsCount = rawFrames.get().length
          val newMaxIndex = maxIndex + 1
          atomicFrameIndex.set(newMaxIndex)
          frameDrawnFlag.set(false)
          if (newMaxIndex > drawActionsCount) {
            animatingFlag.set(false)
            this.cancel()
            atomicFrameAdvanceTask.set(None)
          }
        }
        scrollPane.repaint()
      }
    }
  }

  private def scheduleFrameAdvanceTask(): Unit = {
    val frameAdvanceTask = createFrameAdvanceTask
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
      atomicFrameIndex.set(0)
      scheduleFrameAdvanceTask()
    }
    scrollPane.repaint()
  }

  def continueAnimation(): Unit = {
    if (!animatingFlag.get()) {
      animatingFlag.set(true)
      scheduleFrameAdvanceTask()
    }
    scrollPane.repaint()
  }

  def component: JComponent = scrollPane

  def appendFrames(a: Vector[Frame]): Unit =
    SwingUtilities.invokeLater { () =>
      rawFrames.updateAndGet(_ ++ a)
      adjustFrames()
      continueAnimation()
    }

  def setAnimation(a: Animation): Unit =
    SwingUtilities.invokeLater { () =>
      rawFrames.set(a)
      adjustFrames()
      startAnimation()
    }

  def restart(): Unit = SwingUtilities.invokeLater { () =>
    animatingFlag.set(false)
    startAnimation()
    panel.repaint()
  }

  def clear(): Unit = SwingUtilities.invokeLater { () =>
    rawFrames.set(Vector.empty)
    restart()
  }

  def updateDrawDelay(delay: Int): Unit = {
    atomicDelay.set(delay.abs)
    if (animatingFlag.get()) {
      scheduleFrameAdvanceTask()
    }
  }

  private def adjustFrames(): Unit = {
    val frames = rawFrames.get()
    val (minX, minY) = frames.flatten.foldLeft((0d, 0d)) {
      case ((x, y), (shape, _)) =>
        val bounds = shape.getBounds2D
        (x.min(bounds.getMinX), y.min(bounds.getMinY))
    }
    val shift = AffineTransform.getTranslateInstance(-minX + margin, -minY + margin)
    val shifted = frames.map(_.map {
      case (shape, color) =>
        val newShape = shift.createTransformedShape(shape)
        (newShape, color)
    })
    shiftedFrames.set(shifted)
    shifted.flatten match {
      case (f0, _) +: fx =>
        val bounds = fx.foldLeft(f0.getBounds2D)(_ createUnion _._1.getBounds2D)
        panel.setPreferredSize(new Dimension(
          bounds.getWidth.ceil.toInt + margin.toInt,
          bounds.getHeight.ceil.toInt + margin.toInt
        ))
        scrollPane.revalidate()
      case _ => panel.repaint()
    }
  }
}
object Canvas {
  final val DefaultMargin = 20d

  type Frame = Vector[(Shape, Color)]
  type Animation = Vector[Frame]
}