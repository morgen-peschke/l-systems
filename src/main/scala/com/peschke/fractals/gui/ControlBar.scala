package com.peschke.fractals
package gui

import com.peschke.fractals.gui.ControlBar.{ControlSettings, LSystemChoices, SegmentLengthScaleFactor}
import com.peschke.fractals.lsystem.LSystem
import javax.swing._
import javax.swing.border.EtchedBorder

import scala.collection.immutable

class ControlBar(colorBar: ColorBar, initialIterations: Int, initialSegmentLength: Int, initialRenderDelay: Int) {
  private final val panel = new JPanel()
  private final val systemChoiceInput = ControlBar.systemChoicesInput
  private final val iterationsInput = ControlBar.iterationsInput(initialIterations)
  private final val segmentLengthInput = ControlBar.segmentLengthInput(initialSegmentLength)
  private final val renderStyleInput = ControlBar.renderStyleInput
  private final val renderDelayInput = ControlBar.renderDelay(initialRenderDelay)
  private final val buttonPanel = new JPanel()
  private final val renderButton = ControlBar.renderButton
  private final val cancelButton = ControlBar.cancelButton
  cancelButton.setEnabled(false)

  panel.setLayout(new BoxLayout(panel, BoxLayout.LINE_AXIS))
  buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.PAGE_AXIS))
  buttonPanel.add(renderButton)
  buttonPanel.add(cancelButton)

  panel.add(systemChoiceInput)
  panel.add(iterationsInput)
  panel.add(segmentLengthInput)
  panel.add(colorBar.component)
  panel.add(Box.createHorizontalGlue())
  panel.add(renderDelayInput)
  panel.add(renderStyleInput)
  panel.add(buttonPanel)

  panel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED))

  def component: JComponent = panel

  def controlSettings: ControlSettings = {
    val systemChoice =
      Option(systemChoiceInput.getItemAt(systemChoiceInput.getSelectedIndex))
        .getOrElse(LSystemChoices.KochCurve)
    ControlSettings(
      lSystem = systemChoice.init((segmentLengthInput.getValue.toDouble / SegmentLengthScaleFactor.toDouble).max(0.1d)),
      iterations = iterationsInput.getValue
    )
  }

  def registerRenderButtonPressedCallback(callback: ControlSettings => Runnable): Unit =
    renderButton.addActionListener(_.getActionCommand match {
      case ControlBar.RenderAction if renderButton.isEnabled =>
        SwingUtilities.invokeLater(callback(controlSettings))
      case _                                                 => ()
    })

  def registerCancelButtonPressedCallback(callback: Runnable): Unit = cancelButton
    .addActionListener(_.getActionCommand match {
      case ControlBar.CancelAction if cancelButton.isEnabled =>
        SwingUtilities.invokeLater(callback)
      case _                                                 => ()
    })

  def registerRenderDelayUpdateCallback(callback: Int => Runnable): Unit =
    renderDelayInput.addChangeListener { _ =>
      SwingUtilities.invokeLater(callback(renderDelayInput.getValue))
    }

  def registerIterationCountUpdateCallback(callback: Int => Runnable): Unit =
    iterationsInput.addChangeListener { _ =>
      SwingUtilities.invokeLater(callback(iterationsInput.getValue))
    }

  private def setEnabled(enabled: Boolean): Unit = {
    renderButton.setEnabled(enabled)
    cancelButton.setEnabled(!enabled)
    iterationsInput.setEnabled(enabled)
    segmentLengthInput.setEnabled(enabled)
  }

  def disable(): Unit = setEnabled(false)

  def enable(): Unit = setEnabled(true)

  def renderStyle: ControlBar.RenderStyle =
    Option(renderStyleInput.getItemAt(renderStyleInput.getSelectedIndex)).getOrElse(ControlBar.RenderStyle.Static)
}
object ControlBar {
  final val SegmentLengthScaleFactor = 10
  case class ControlSettings(lSystem: LSystem, iterations: Int)

  final val RenderAction = "render"
  final val CancelAction = "cancel"

  sealed abstract class RenderStyle
  object RenderStyle {
    case object Static extends RenderStyle
    case object Animated extends RenderStyle
  }

  sealed abstract class LSystemChoices(val init: Double => LSystem) extends enumeratum.EnumEntry
  object LSystemChoices extends enumeratum.Enum[LSystemChoices] {
    case object KochSnowflake extends LSystemChoices(LSystem.kochSnowflake(_))
    case object KochCurve extends LSystemChoices(LSystem.kochCurve(_))
    case object DragonCurve extends LSystemChoices(LSystem.dragonCurve(_))
    case object SierpinskiTriangle extends LSystemChoices(LSystem.sierpinskiTriangle(_))
    case object SierpinskiArrowHead extends LSystemChoices(LSystem.sierpinskiArrowHead(_))

    override def values: immutable.IndexedSeq[LSystemChoices] = findValues
  }

  def systemChoicesInput: JComboBox[LSystemChoices] = {
    val box = new JComboBox[LSystemChoices]()
    box.setEditable(false)
    LSystemChoices.values.foreach(box.addItem)
    box.setSelectedItem(LSystemChoices.KochCurve)
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

  def iterationsInput(initialValue: Int): JSlider = {
    val input = new JSlider()
    input.setMinimum(0)
    input.setMaximum(25)
    input.setValue(initialValue.min(25).max(0))
    input.setMajorTickSpacing(5)
    input.setMinorTickSpacing(1)
    input.setPaintTicks(true)
    input.setPaintLabels(true)
    input.setBorder(BorderFactory.createTitledBorder("Iterations"))
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
    input.setBorder(BorderFactory.createTitledBorder("Render Delay"))
    input
  }

  def renderStyleInput: JComboBox[RenderStyle] = {
    val box = new JComboBox[RenderStyle]()
    box.setEditable(false)
    box.addItem(RenderStyle.Static)
    box.addItem(RenderStyle.Animated)
    box.setSelectedItem(RenderStyle.Static)
    box.setBorder(BorderFactory.createTitledBorder("Animation Style"))
    box
  }
}
