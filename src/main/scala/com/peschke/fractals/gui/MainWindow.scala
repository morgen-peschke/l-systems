package com.peschke.fractals.gui

import java.awt.{GridBagConstraints, GridBagLayout}

import javax.swing._

class MainWindow(controlBar: ControlBar, canvas: Canvas) {
  private val frame = new JFrame()
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  frame.setLayout(new GridBagLayout)

  frame.add(controlBar.component, MainWindow.controlBarConstraints)
  frame.add(canvas.component, MainWindow.canvasConstraints)

  frame.pack()
  frame.setSize(frame.getGraphicsConfiguration.getBounds.getSize)
  frame.setVisible(true)
}
object MainWindow {
  def controlBarConstraints: GridBagConstraints = ConstraintFactory.gridBag(
    _.anchor = GridBagConstraints.PAGE_START,
    _.fill = GridBagConstraints.HORIZONTAL,
    _.weightx = 1.0,
    _.weighty = 0.0
  )

  def canvasConstraints: GridBagConstraints =
    ConstraintFactory.gridBag(
      _.gridy = 1,
      _.weightx = 1.0,
      _.weighty = 1.0
    )
}
