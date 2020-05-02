package com.peschke.fractals.gui

import java.awt.{GridBagConstraints, GridBagLayout, Insets}

import javax.swing.{BorderFactory, JButton, JComponent, JPanel, JProgressBar, JSlider, SwingUtilities}

class IterationControl(initialIterations: Int) {
  private val panel = new JPanel()
  private val slider = IterationControl.sliderBar(initialIterations)
  private val calculateProgressBar = IterationControl.progressBar(initialIterations)
  private val renderProgressBar = IterationControl.progressBar(initialIterations)
  private val displayProgressBar = IterationControl.progressBar(initialIterations)
  private val upButton = IterationControl.upButton
  private val downButton = IterationControl.downButton

  panel.setLayout(new GridBagLayout)
  panel.setBorder(BorderFactory.createTitledBorder("Iterations"))

  panel.add(upButton, IterationControl.upButtonConstraints)
  panel.add(downButton, IterationControl.downButtonConstraints)
  panel.add(slider, IterationControl.sliderBarConstraints)
  panel.add(calculateProgressBar, IterationControl.calculationProgressBarConstraints)
  panel.add(renderProgressBar, IterationControl.renderProgressBarConstraints)
  panel.add(displayProgressBar, IterationControl.displayProgressBarConstraints)

  def component: JComponent = panel

  def drawingProgressBar: JProgressBar = displayProgressBar

  def iterations: Int = slider.getValue

  def registerUpdateCallback(callback: Int => Runnable): Unit =
    slider.addChangeListener { _ =>
      SwingUtilities.invokeLater(callback(slider.getValue))
    }

  registerUpdateCallback(iterations => () => {
    calculateProgressBar.setMaximum(iterations.max(1))
    calculateProgressBar.setValue(iterations.max(1))
    calculateProgressBar.repaint()
  })

  upButton.addActionListener { _ =>
    if (upButton.isEnabled) {
      slider.setValue((slider.getValue + 1).min(slider.getMaximum))
    }
  }

  downButton.addActionListener { _ =>
    if (downButton.isEnabled) {
      slider.setValue((slider.getValue - 1).max(slider.getMinimum))
    }
  }

  def setEnabled(enabled: Boolean): Unit = {
    slider.setEnabled(enabled)
    upButton.setEnabled(enabled)
    downButton.setEnabled(enabled)
  }

  def resetProgress(): Unit = SwingUtilities.invokeLater { () =>
    calculateProgressBar.setValue(0)
    renderProgressBar.setValue(0)
    displayProgressBar.setValue(0)
  }

  def updateCalculateProgress(value: Int): Unit =
    SwingUtilities.invokeLater(() => {
      calculateProgressBar.setValue(value.min(calculateProgressBar.getMaximum))
      calculateProgressBar.repaint()
    })

  def updateRenderProgress(value: Int): Unit =
    SwingUtilities.invokeLater(() => {
      renderProgressBar.setValue(value.min(renderProgressBar.getMaximum))
      renderProgressBar.repaint()
    })

  def updateDisplayProgress(value: Int): Unit =
    SwingUtilities.invokeLater(() => {
      displayProgressBar.setValue(value.min(displayProgressBar.getMaximum))
      displayProgressBar.repaint()
    })

  def setRenderProgressMax(max: Int): Unit =
    SwingUtilities.invokeLater { () =>
      renderProgressBar.setMaximum(max)
    }

  def setDisplayProgressMax(max: Int): Unit =
    SwingUtilities.invokeLater { () =>
      displayProgressBar.setMaximum(max)
    }

  def maxOutCalculateProgress(): Unit = updateCalculateProgress(calculateProgressBar.getMaximum)

  def maxOutRenderProgress(): Unit = updateCalculateProgress(renderProgressBar.getMaximum)

  def maxOutDisplayProgress(): Unit = updateCalculateProgress(displayProgressBar.getMaximum)
}
object IterationControl {

  def downButtonConstraints: GridBagConstraints =
    ConstraintFactory.gridBag(
      _.anchor = GridBagConstraints.CENTER,
      _.fill = GridBagConstraints.NONE,
      _.weightx = 0.0,
      _.weighty = 0.0,
      _.gridx = 0,
      _.gridy = 0
    )

  def sliderBarConstraints: GridBagConstraints =
    ConstraintFactory.gridBag(
      _.gridx = 1,
      _.gridy = 0,
      _.gridheight = 1,
      _.gridwidth = 2
    )

  def upButtonConstraints: GridBagConstraints =
    ConstraintFactory.gridBag(
      _.anchor = GridBagConstraints.CENTER,
      _.fill = GridBagConstraints.NONE,
      _.weightx = 0.0,
      _.weighty = 0.0,
      _.gridx = 4,
      _.gridy = 0
    )

  def calculationProgressBarConstraints: GridBagConstraints =
    ConstraintFactory.gridBag(
      _.fill = GridBagConstraints.HORIZONTAL,
      _.anchor = GridBagConstraints.PAGE_END,
      _.gridx = 0,
      _.gridy = 3,
      _.gridwidth = 5
    )

  def renderProgressBarConstraints: GridBagConstraints =
    ConstraintFactory.gridBag(
      _.fill = GridBagConstraints.HORIZONTAL,
      _.anchor = GridBagConstraints.PAGE_END,
      _.gridx = 0,
      _.gridy = 4,
      _.gridwidth = 5
    )

  def displayProgressBarConstraints: GridBagConstraints =
    ConstraintFactory.gridBag(
      _.fill = GridBagConstraints.HORIZONTAL,
      _.anchor = GridBagConstraints.PAGE_END,
      _.gridx = 0,
      _.gridy = 5,
      _.gridwidth = 5
    )

  def sliderBar(initialValue: Int): JSlider = {
    val input = new JSlider()
    input.setMinimum(0)
    input.setMaximum(25)
    input.setValue(initialValue.min(25).max(0))
    input.setMajorTickSpacing(5)
    input.setMinorTickSpacing(1)
    input.setPaintTicks(true)
    input.setPaintLabels(true)
    input.setSnapToTicks(true)
    input
  }

  def progressBar(initialMax: Int): JProgressBar = {
    val bar = new JProgressBar()
    bar.setMinimum(0)
    bar.setMaximum(initialMax)
    bar.setValue(0)
    bar
  }

  def upButton: JButton = {
    val b = new JButton("\u21e8")
    b.setMargin(new Insets(0, 0, 0, 0))
    b
  }

  def downButton: JButton = {
    val b = new JButton("\u21e6")
    b.setMargin(new Insets(0, 0, 0, 0))
    b
  }
}
