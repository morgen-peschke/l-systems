package com.peschke.fractals.gui

import java.awt.{Color, GridBagConstraints, GridBagLayout}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import javax.swing._

class ColorBar(initStartColor: Color, initEndColor: Color, initStepCount: Int) {
  private final val container = new JPanel()
  private final val leftColumn = new JPanel()
  private final val rightColumn = new JPanel()
  private final val colorStepsInput = IterationControl.sliderBar(10)

  (container :: leftColumn :: rightColumn :: Nil).foreach { c =>
    c.setLayout(new GridBagLayout)
    c.setVisible(true)
  }
  container.setBorder(BorderFactory.createLineBorder(Color.black))
  container.add(leftColumn, ColorBar.colConstraints(0))
  container.add(rightColumn, ColorBar.colConstraints(1))
  container.add(colorStepsInput, ColorBar.stepCountConstraints)

  private final val atomicStartColor = new AtomicReference[Color](initStartColor)
  private final val atomicEndColor = new AtomicReference[Color](initEndColor)
  private final val atomicStepCount = new AtomicInteger(initStepCount)
  private final val atomicColorList = new AtomicReference[Vector[Color]](Vector.empty)
  private final val bandedCheckbox = new JCheckBox("Banded", false)
  private final val startColorButton = ColorBar.colorPickerButton("Start Color")
  private final val endColorButton = ColorBar.colorPickerButton("End Color")
  leftColumn.add(startColorButton, ColorBar.colorPickerButtonConstraints(0))
  leftColumn.add(endColorButton, ColorBar.colorPickerButtonConstraints(1))
  leftColumn.add(bandedCheckbox, ColorBar.colorPickerButtonConstraints(2))

  colorStepsInput.addChangeListener { _ =>
    atomicStepCount.set(colorStepsInput.getValue)
    rebuildColorList()
  }

  private def rebuildColorList(): Unit = {
    rightColumn.getComponents
      .filter(_.getName == ColorBar.ColorSquareName)
      .foreach(rightColumn.remove)
    val colors =
      ColorBar
        .calculateColorBand(
          atomicStartColor.get,
          atomicEndColor.get,
          atomicStepCount.get,
          bandedCheckbox.isSelected
        )
    atomicColorList.set(colors)
    colors.zipWithIndex
      .foreach {
        case (color, i) =>
          rightColumn.add(ColorBar.colorSquare(color),
            ColorBar.colorSquareConstraints(i))
      }
    container.validate()
  }

  startColorButton.addActionListener(_ =>
    Option(
      JColorChooser.showDialog(startColorButton,
        "Choose Start Color",
        atomicStartColor.get()))
      .foreach { newColor =>
        atomicStartColor.set(newColor)
        rebuildColorList()
      }
  )

  endColorButton.addActionListener(_ =>
    Option(
      JColorChooser.showDialog(endColorButton,
        "Choose End Color",
        atomicEndColor.get()))
      .foreach { newColor =>
        atomicEndColor.set(newColor)
        rebuildColorList()
      }
  )

  bandedCheckbox.addItemListener(_ => rebuildColorList())

  rebuildColorList()

  def updateStepCount(stepCount: Int): Unit = {
    atomicStepCount.set(stepCount.max(1))
    rebuildColorList()
  }

  def component: JComponent = container

  def colors: Vector[Color] = atomicColorList.get()

  def endColor: Color = atomicEndColor.get()
}
object ColorBar {
  def interpolate(start: Float, end: Float, steps: Int): Vector[Float] = {
    val step = (end - start) / steps.toFloat
    Iterator.iterate(end)(_ - step)
      .take(steps)
      .toVector
      .reverse
  }

  def banded[A](input: Vector[A]): Vector[A] =
    if (input.length <= 3) input
    else
      input
        .zip(input.drop(3) ++ input.take(3))
        .flatMap {
          case (a, b) => Vector(a, b)
        }
        .take(input.length)

  def gradient(start: Color, end: Color, steps: Int): Vector[Color] = {
    val Array(rs, gs, bs, as) = start.getRGBComponents(Option.empty.orNull)
    val Array(re, ge, be, ae) = end.getRGBComponents(Option.empty.orNull)
    val reds = interpolate(rs, re, steps)
    val greens = interpolate(gs, ge, steps)
    val blues = interpolate(bs, be, steps)
    val alphas = interpolate(as, ae, steps)
    val result = (reds zip greens zip blues zip alphas).map {
      case (((red, green), blue), alpha) => new Color(red, green, blue, alpha)
    }
    result match {
      case Vector() | Vector(_) => Vector(start)
      case _ +: middle :+ _     => start +: middle :+ end
    }
  }

  def calculateColorBand(start: Color, end: Color, steps: Int, isBanded: Boolean): Vector[Color] = {
    val gradientBand = gradient(start, end, steps)
    if (isBanded) banded(gradientBand)
    else gradientBand
  }

  final val ColorSquareName =
    "ColorSquareName"

  def colorSquare(color: Color): JPanel = {
    val cs = new JPanel
    cs.setName(ColorSquareName)
    cs.setBackground(color)
    cs.setVisible(true)
    cs
  }

  def colorPickerButton(text: String): JButton = {
    val b = new JButton(text)
    b.setVisible(true)
    b
  }

  def colConstraints(index: Int): GridBagConstraints =
    ConstraintFactory.gridBag(
      _.anchor = GridBagConstraints.PAGE_START,
      _.fill = GridBagConstraints.BOTH,
      _.gridx = index,
      _.weightx = index.toFloat
    )

  def colorSquareConstraints(index: Int): GridBagConstraints =
    ConstraintFactory.gridBag(
      _.anchor = GridBagConstraints.LAST_LINE_START,
      _.gridx = index
    )

  def colorPickerButtonConstraints(index: Int): GridBagConstraints =
    ConstraintFactory.gridBag(
      _.anchor = GridBagConstraints.LINE_START,
      _.fill = GridBagConstraints.HORIZONTAL,
      _.gridy = index
    )

  def stepCountConstraints: GridBagConstraints =
    ConstraintFactory.gridBag(
      _.gridx = 0,
      _.gridy = 2,
      _.gridwidth = 2
    )
}
