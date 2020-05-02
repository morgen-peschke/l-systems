package com.peschke.fractals
package gui

import cats.instances.vector._
import cats.syntax.foldable._
import com.peschke.fractals.gui.ControlBar.{AnimationStyle, ControlSettings, SegmentLengthScaleFactor}
import com.peschke.fractals.lsystem.LSystem
import com.peschke.fractals.turtle.{Angle, Distance, Turtle}
import javax.swing._
import javax.swing.border.EtchedBorder

import scala.collection.immutable

class ControlBar(colorBar: ColorBar, iterationControl: IterationControl, initialSegmentLength: Int,
                 initialRenderDelay: Int) {
  private final val panel = new JPanel()
  private final val systemPanel = new JPanel()
  private final val buttonPanel = new JPanel()
  private final val renderPanel = new JPanel()

  private final val systemChoiceInput = ControlBar.systemChoicesInput
  private final val segmentLengthInput = ControlBar.segmentLengthInput(initialSegmentLength)
  private final val animationStyleInput = ControlBar.renderStyleInput
  private final val renderDelayInput = ControlBar.renderDelay(initialRenderDelay)
  private final val renderButton = ControlBar.renderButton

  buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.PAGE_AXIS))
  buttonPanel.add(renderButton)

  systemPanel.setLayout(new BoxLayout(systemPanel, BoxLayout.PAGE_AXIS))
  systemPanel.add(systemChoiceInput)
  systemPanel.add(segmentLengthInput)

  renderPanel.setLayout(new BoxLayout(renderPanel, BoxLayout.PAGE_AXIS))
  renderPanel.add(animationStyleInput)
  renderPanel.add(renderDelayInput)

  panel.setLayout(new BoxLayout(panel, BoxLayout.LINE_AXIS))
  panel.add(systemPanel)
  panel.add(iterationControl.component)
  panel.add(colorBar.component)
  panel.add(Box.createHorizontalGlue())
  panel.add(renderPanel)
  panel.add(buttonPanel)

  panel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED))

  def component: JComponent = panel

  def controlSettings: ControlSettings = {
    val systemChoice =
      Option(systemChoiceInput.getItemAt(systemChoiceInput.getSelectedIndex))
        .orElse(LSystem.values.headOption)
        .getOrElse {
          throw new IllegalArgumentException("No L-Systems configured")
        }
    ControlSettings(
      lSystem = systemChoice,
      segmentLength = Distance((segmentLengthInput.getValue.toDouble / SegmentLengthScaleFactor.toDouble).max(0.1d)),
      iterations = iterationControl.iterations
    )
  }

  def watchRenderButtonPressed(callback: ControlSettings => Runnable): Unit =
    renderButton.addActionListener(_.getActionCommand match {
      case ControlBar.RenderAction =>
        SwingUtilities.invokeLater(callback(controlSettings))
      case _                       => ()
    })

  def watchCancelButtonPressed(callback: Runnable): Unit = renderButton
    .addActionListener(_.getActionCommand match {
      case ControlBar.CancelAction =>
        SwingUtilities.invokeLater(callback)
      case _                       => ()
    })

  def watchRenderDelayUpdate(callback: Int => Runnable): Unit =
    renderDelayInput.addChangeListener { _ =>
      SwingUtilities.invokeLater(callback(renderDelayInput.getValue))
    }

  def watchAnimationStyleUpdate(callback: PartialFunction[AnimationStyle, Runnable]): Unit = animationStyleInput.addItemListener { _ =>
    callback.andThen(SwingUtilities.invokeLater).applyOrElse(animationStyle, (_: AnimationStyle) => ())
  }

  private def setEnabled(enabled: Boolean): Unit = {
    if (enabled) {
      renderButton.setText("Render")
      renderButton.setActionCommand(ControlBar.RenderAction)
    } else {
      renderButton.setText("Cancel")
      renderButton.setActionCommand(ControlBar.CancelAction)
    }
    renderButton.repaint()
    iterationControl.setEnabled(enabled)
    segmentLengthInput.setEnabled(enabled)
  }

  def disable(): Unit = setEnabled(false)

  def enable(): Unit = setEnabled(true)

  def animationStyle: ControlBar.AnimationStyle =
    Option(animationStyleInput.getItemAt(animationStyleInput.getSelectedIndex))
      .getOrElse(ControlBar.AnimationStyle.Static)
}
object ControlBar {
  final val SegmentLengthScaleFactor = 10
  case class ControlSettings(lSystem: LSystem, segmentLength: Distance, iterations: Int)

  final val RenderAction = "render"
  final val CancelAction = "cancel"

  sealed abstract class AnimationStyle extends enumeratum.EnumEntry {
    def render(distance: Distance,
               angle: Angle,
               commands: Vector[LSystem.Command],
               canvas: Canvas,
               reportProgress: Int => Unit): Unit
  }
  object AnimationStyle extends enumeratum.Enum[AnimationStyle] {
    def prepare(distance: Distance,
                angle: Angle,
                commands: Vector[LSystem.Command],
                reportProgress: Int => Unit
               ): Vector[Canvas.Element] =
      commands
        .map(LSystem.Command.renderer(distance, angle))
        .zipWithIndex
        .foldMapM {
          case (algebra, index) =>
            reportProgress(index)
            Turtle.renderingInterpreter(algebra)
        }
        .runA(Turtle.origin)
        .value

    case object Static extends AnimationStyle {
      override def render(distance: Distance,
                          angle: Angle,
                          commands: Vector[LSystem.Command],
                          canvas: Canvas,
                          reportProgress: Int => Unit): Unit =
        canvas.replaceElementsImmediately(prepare(distance, angle, commands, reportProgress))
    }
    case object Animated extends AnimationStyle {
      override def render(distance: Distance,
                          angle: Angle,
                          commands: Vector[LSystem.Command],
                          canvas: Canvas,
                          reportProgress: Int => Unit): Unit = {
        canvas.clear()
        canvas.appendElements(prepare(distance, angle, commands, reportProgress))
      }
    }
    override def values: immutable.IndexedSeq[AnimationStyle] = findValues
  }

  def systemChoicesInput: JComboBox[LSystem] = {
    val box = new JComboBox[LSystem]()
    box.setEditable(false)
    LSystem.values.foreach(box.addItem)
    box.setSelectedIndex(0)
    box.setBorder(BorderFactory.createTitledBorder("L-System"))
    box
  }

  def renderButton: JButton = {
    val b = new JButton("Render")
    b.setActionCommand(RenderAction)
    b
  }

  def cancelButton: JButton = {
    val b = new JButton("Cancel")
    b.setActionCommand(CancelAction)
    b.setMultiLineToolTip {
      s"""|Cancel a render in progress.
          |
          |If animation is selected as the render style, it
          |generally takes less time to render than to display
          |the full animation.
          |
          |This button will not cancel the animation, just
          |interrupt the render calculations.""".stripMargin
    }
    b
  }

  def segmentLengthInput(initialValue: Int): JSlider = {
    val input = new JSlider()
    input.setMinimum(11)
    input.setMaximum(1000)
    input.setValue((initialValue * SegmentLengthScaleFactor).min(1000).max(11))
    input.setPaintTicks(false)
    input.setPaintLabels(false)

    def generateTitle: String =
      "Segment Length: %.2f".format(input.getValue.toDouble / SegmentLengthScaleFactor.toDouble)

    val border = BorderFactory.createTitledBorder(generateTitle)
    input.setBorder(border)
    input.addChangeListener { _ =>
      border.setTitle(generateTitle)
      input.repaint()
    }
    input
  }

  def renderDelay(initialValue: Int): JSlider = {
    val input = new JSlider()
    input.setMinimum(0)
    input.setMaximum(2000)
    input.setValue(initialValue.min(2000).max(0))
    input.setMajorTickSpacing(500)
    input.setMinorTickSpacing(100)
    input.setPaintTicks(true)
    input.setPaintLabels(true)
    input.setBorder(BorderFactory.createTitledBorder("Extra Render Delay"))
    input
  }

  def renderStyleInput: JComboBox[AnimationStyle] = {
    val box = new JComboBox[AnimationStyle]()
    box.setEditable(false)
    AnimationStyle.values.foreach(box.addItem)
    box.setSelectedItem(AnimationStyle.Static)
    box.setBorder(BorderFactory.createTitledBorder("Animation Style"))
    box
  }
}
