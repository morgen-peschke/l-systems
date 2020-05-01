package com.peschke.fractals

import java.awt.Color
import java.util.concurrent.atomic.AtomicReference

import com.peschke.fractals.gui.ControlBar.{AnimationStyle, LSystemChoice}
import com.peschke.fractals.gui.{Canvas, ColorBar, ControlBar, MainWindow}
import com.peschke.fractals.rendering.{ColoredIterationWorker, RenderLogic}
import javax.swing.{SwingUtilities, SwingWorker}

object Main extends App {
  SwingUtilities.invokeLater { () =>
    val colorBar = new ColorBar(
      initStartColor = Color.white,
      initEndColor = Color.black,
      initStepCount = 5
    )
    val controlBar = new ControlBar(
      colorBar = colorBar,
      initialIterations = 5,
      initialSegmentLength = 5,
      initialRenderDelay = 0
    )
    val canvas = new Canvas(defaultDelay = 10)
    controlBar.registerRenderDelayUpdateCallback { delay =>
      () => canvas.updateDrawDelay(delay)
    }
    controlBar.registerIterationCountUpdateCallback { iterations =>
      () => colorBar.updateStepCount(iterations)
    }
    val atomicWorker = new AtomicReference[Option[SwingWorker[Unit, Unit]]](None)
    controlBar.registerCancelButtonPressedCallback { () =>
      atomicWorker.getAndSet(None).foreach { oldWorker =>
        oldWorker.cancel(true)
        canvas.clear()
      }
    }

    controlBar.registerRenderButtonPressedCallback { settings =>
      () =>
        controlBar.disable()
        canvas.clear()
        val renderLogic = controlBar.animationStyle match {
          case AnimationStyle.Static   => RenderLogic.static(canvas)
          case AnimationStyle.Animated =>
            settings.lSystemChoice match {
              case LSystemChoice.`Sierpinski ArrowHead` =>
                RenderLogic.renderFirst1OfEveryNIterations(2, canvas)

              case _ => RenderLogic.simple(canvas)
            }
        }
        val worker =
          new ColoredIterationWorker(
            system = settings.lSystem,
            iterations = renderLogic.adjustColorVector(colorBar.colors),
            logic = renderLogic,
            finish = () => controlBar.enable()
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
