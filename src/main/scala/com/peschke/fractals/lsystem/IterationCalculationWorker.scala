package com.peschke.fractals.lsystem

import com.peschke.fractals.lsystem.LSystem.Command
import javax.swing.SwingWorker

class IterationCalculationWorker(system: LSystem,
                                 iterations: Int,
                                 trackProgress: Int => Unit,
                                 finish: Vector[Command] => Unit)
  extends SwingWorker[Unit, Unit] {
  override def doInBackground(): Unit = {
    @scala.annotation.tailrec
    def loop(state: Vector[Command], iteration: Int): Vector[Command] = {
      if (isCancelled) Vector.empty
      else if (iteration >= iterations) state
           else {
             val next = system.next(state)
             trackProgress(iteration)
             loop(next, iteration + 1)
           }
    }

    finish {
      try loop(system.defaultSeed, 0)
      catch {
        case _: InterruptedException => Vector.empty
      }
    }
  }
}
