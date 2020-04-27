package com.peschke.fractals.rendering

import java.awt.Color

import com.peschke.fractals.lsystem.LSystem
import com.peschke.fractals.lsystem.LSystem.Element
import javax.swing.SwingWorker

class ColoredIterationWorker(
                                 system: LSystem,
                                 iterations: Vector[Color],
                                 processIteration: (Vector[Element], Color) => Unit,
                                 finish: () => Unit
                               )
  extends SwingWorker[Unit, Unit] {
  override def doInBackground(): Unit = {
    iterations
      .foldLeft(system.defaultSeed) {
        case (ignored, _) if isCancelled => ignored
        case (state, color)              =>
          processIteration(state, color)
          system.next(state)
      }
    ()
  }

  override def done(): Unit = {
    finish()
  }
}