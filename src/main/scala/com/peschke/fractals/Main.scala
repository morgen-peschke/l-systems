package com.peschke.fractals

import java.awt.Color
import java.util.concurrent.atomic.AtomicReference

import com.peschke.fractals.gui._
import com.peschke.fractals.lsystem.IterationCalculationWorker
import javax.swing.{SwingUtilities, SwingWorker}

object Main extends App {
  SwingUtilities.invokeLater { () =>
    val colorBar = new ColorBar(
      initStartColor = Color.white,
      initEndColor = Color.black,
      initStepCount = 5
    )
    val iterationControl = new IterationControl(5)
    val controlBar = new ControlBar(
      colorBar = colorBar,
      iterationControl = iterationControl,
      initialSegmentLength = 5,
      initialRenderDelay = 0
    )
    val canvas = new Canvas(
      defaultDelay = 10,
      drawProgressBar = iterationControl.drawingProgressBar
    )
    controlBar.watchRenderDelayUpdate { delay =>
      () => canvas.updateDrawDelay(delay)
    }
    iterationControl.registerUpdateCallback { iterations =>
      () => colorBar.updateStepCount(iterations)
    }
    val atomicWorker = new AtomicReference[Option[SwingWorker[Unit, Unit]]](None)

    controlBar.watchCancelButtonPressed { () =>
      atomicWorker.getAndSet(None).foreach { oldWorker =>
        oldWorker.cancel(true)
        canvas.clear()
      }
    }

    controlBar.watchRenderButtonPressed { settings =>
      () =>
        controlBar.disable()
        iterationControl.resetProgress()
        canvas.clear()

        val worker =
          new IterationCalculationWorker(
            system = settings.lSystem,
            iterations = settings.iterations,
            trackProgress = iterationControl.updateCalculateProgress,
            finish = result => {
              iterationControl.maxOutCalculateProgress()
              iterationControl.setRenderProgressMax(result.length)
              controlBar
                .animationStyle
                .render(
                  settings.segmentLength,
                  settings.lSystem.defaultAngle,
                  result,
                  canvas,
                  iterationControl.updateRenderProgress
                )
              iterationControl.maxOutRenderProgress()
              controlBar.enable()
            }
          )

        atomicWorker.getAndSet(Some(worker)).foreach { oldWorker =>
          oldWorker.cancel(false)
          canvas.clear()
        }
        worker.execute()
    }

    new MainWindow(controlBar, canvas)
    ()
  }
}
