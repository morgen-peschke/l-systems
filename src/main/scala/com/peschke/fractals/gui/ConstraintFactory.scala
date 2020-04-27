package com.peschke.fractals.gui

import java.awt.GridBagConstraints

object ConstraintFactory {
  def gridBag(updates: (GridBagConstraints => Any)*): GridBagConstraints = {
    val constraints = new GridBagConstraints()
    constraints.anchor = GridBagConstraints.CENTER
    constraints.fill = GridBagConstraints.BOTH
    constraints.gridx = 0
    constraints.gridy = 0
    constraints.weightx = 0.5
    constraints.weighty = 0.5
    updates.foreach(_.apply(constraints))
    constraints
  }
}
