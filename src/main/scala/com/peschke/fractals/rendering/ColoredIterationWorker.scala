package com.peschke.fractals.rendering

import java.awt.Color

import com.peschke.fractals.lsystem.LSystem
import javax.swing.SwingWorker

class ColoredIterationWorker(system: LSystem,
                             iterations: Vector[Color],
                             logic: RenderLogic,
                             finish: () => Unit
                            )
  extends SwingWorker[Unit, Unit] {
  override def doInBackground(): Unit = {
    iterations
      .foldLeft(system.defaultSeed) {
        case (ignored, _) if isCancelled => ignored
        case (state, color)              =>
          logic.processIteration(state, color)
          system.next(state)
      }
    ()
  }

  override def done(): Unit = {
    logic.cleanup()
    finish()
  }
}