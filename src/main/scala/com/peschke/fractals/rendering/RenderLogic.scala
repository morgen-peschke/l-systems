package com.peschke.fractals.rendering

import java.awt.Color

import cats.syntax.eq._
import cats.syntax.foldable._
import cats.instances.int._
import cats.instances.vector._
import com.peschke.fractals.gui.Canvas
import com.peschke.fractals.lsystem.LSystem
import com.peschke.fractals.lsystem.LSystem.Element
import com.peschke.fractals.turtle.{Angle, Distance, Turtle}

case class RenderLogic(processIteration: (Vector[Element], Color) => Unit,
                       adjustColorVector: Vector[Color] => Vector[Color] = identity,
                       cleanup: () => Unit = () => ())

object RenderLogic {
  def static(canvas: Canvas, length: Distance, angle: Angle): RenderLogic = {
    var finalFrame: Vector[Canvas.Element] = Vector.empty
    val renderer = LSystem.Element.renderer(length, angle)
    RenderLogic(
      processIteration = (state, color) => {
        finalFrame =
          state
            .map(renderer)
            .foldMapM(Turtle.renderingInterpreter)
            .runA(Turtle.origin.setColor(color))
            .value
      },
      cleanup = () => canvas.replaceElementsImmediately(finalFrame)
    )
  }

  def simple(canvas: Canvas, length: Distance, angle: Angle): RenderLogic =
    renderFirst1OfEveryNIterations(1, canvas, length, angle)

  def renderFirst1OfEveryNIterations(n: Int, canvas: Canvas, length: Distance, angle: Angle): RenderLogic = {
    var frameCount = 0
    var dropCount = 0
    val renderer = LSystem.Element.renderer(length, angle)
    RenderLogic(
      processIteration = (state, color) => {
        if (dropCount === 0) {
          val raw =
            state
              .map(renderer)
              .foldMapM(Turtle.animationCreationInterpreter)
              .runA(Turtle.origin.setColor(color))
              .value
          val animation = raw.drop(frameCount - 1)
          frameCount += animation.length
          canvas.appendElements(animation)
        }
        dropCount = (dropCount + 1) % n
      },
      adjustColorVector = raw =>
        raw
          .iterator
          .grouped(n)
          .flatMap { cs =>
            Iterator.fill(n)(cs.headOption).flatten
          }
          .take(raw.length)
          .toVector,
    )
  }
}