package com.peschke.fractals

import java.awt.Color
import java.util.concurrent.atomic.AtomicReference

import cats.instances.vector._
import cats.syntax.foldable._
import com.peschke.fractals.gui.ControlBar.RenderStyle
import com.peschke.fractals.gui.{Canvas, ColorBar, ControlBar, MainWindow}
import com.peschke.fractals.lsystem.LSystem
import com.peschke.fractals.rendering.ColoredIterationWorker
import com.peschke.fractals.turtle.Turtle
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
        type IterationProcessor = (Vector[LSystem.Element], Color) => Unit
        val (iterationProcessor, processorCleanup): (IterationProcessor, () => Unit)
        = controlBar.renderStyle match {
          case RenderStyle.Static   =>
            var finalFrame: Canvas.Animation = Vector.empty
            val processor: IterationProcessor = (state, color) => {
              finalFrame = Vector {
                state
                  .map(LSystem.Element.renderer)
                  .foldMapM(Turtle.renderingInterpreter)
                  .runA(Turtle.origin.setColor(color))
                  .value
              }
            }
            (processor, () => canvas.setAnimation(finalFrame))
          case RenderStyle.Animated =>
            var frameCount = 0
            val processor: IterationProcessor = (state, color) => {
              val animation =
                state
                  .map(LSystem.Element.renderer)
                  .foldMapM(Turtle.animationCreationInterpreter)
                  .runA(Turtle.origin.setColor(color))
                  .value
                  .drop(frameCount - 1)
              frameCount += animation.length
              canvas.appendFrames(animation)
            }
            (processor, () => ())
        }
        val worker =
          new ColoredIterationWorker(
            system = settings.lSystem,
            iterations = colorBar.colors,
            processIteration = iterationProcessor,
            finish = () => {
              processorCleanup()
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
