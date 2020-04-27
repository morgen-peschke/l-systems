package com.peschke.fractals.turtle

import java.awt.geom.Line2D
import java.awt.{BasicStroke, Color, Stroke}

import cats.data.State
import cats.syntax.applicative._
import cats.instances.vector._
import com.peschke.fractals.gui.Canvas
import com.peschke.fractals.turtle.Turtle.Pen

import scala.language.higherKinds

case class Turtle(pen: Pen, location: Point, angle: Angle) {
  def turnLeft(delta: Angle): Turtle = copy(angle = angle - delta)

  def turnRight(delta: Angle): Turtle = copy(angle = angle + delta)

  def forward(distance: Double): Turtle =
    copy(location = Point(
      x = location.x + (distance * Math.cos(angle.radians)),
      y = location.y + (distance * Math.sin(angle.radians))
    ))

  def forwardFrame(distance: Double): (Turtle, Canvas.Frame) =
    Turtle.lineBetween(this, forward(distance))

  def penUp: Turtle = copy(pen = pen.copy(state = Turtle.Pen.State.Up))

  def penDown: Turtle = copy(pen = pen.copy(state = Turtle.Pen.State.Down))

  def setColor(color: Color): Turtle = copy(pen = pen.copy(color = color))
}
object Turtle {
  def origin: Turtle = Turtle(Pen.default, Point.Origin, Angle.deg(0))

  def start(point: Point): Turtle =
    Turtle(Pen.default, point, Angle.deg(0))

  case class Pen(state: Pen.State, color: Color, stroke: Stroke)
  object Pen {
    val default: Pen = Pen(State.Down, Color.black, new BasicStroke())
    sealed trait State
    object State {
      case object Up extends State
      case object Down extends State
    }
  }

  def lineBetween(start: Turtle, end: Turtle): (Turtle, Canvas.Frame) = {
    val frame: Canvas.Frame = start.pen.state match {
      case Pen.State.Up   => Vector.empty
      case Pen.State.Down => ()
        (new Line2D.Double(
          start.location.x,
          start.location.y,
          end.location.x,
          end.location.y
        ), start.pen.color).pure[Vector]
    }
    (end, frame)
  }

  sealed abstract class Algebra
  object Algebra {
    final case class Forward(distance: Double) extends Algebra
    final case class TurnLeft(delta: Angle) extends Algebra
    final case class TurnRight(delta: Angle) extends Algebra
    case object PenUp extends Algebra
    case object PenDown extends Algebra
    final case class SetColor(color: Color) extends Algebra
  }

  type Interpreter[F[_], A] = Algebra => F[A]
  type Renderer[Element] = Element => Algebra

  type TurtleState[A] = State[Turtle, A]

  val renderingInterpreter: Interpreter[TurtleState, Canvas.Frame] = {
    case Algebra.Forward(distance) => State(turtle => turtle.forwardFrame(distance))
    case Algebra.TurnRight(delta)  => State(turtle => (turtle.turnRight(delta), Vector.empty))
    case Algebra.TurnLeft(delta)   => State(turtle => (turtle.turnLeft(delta), Vector.empty))
    case Algebra.PenUp             => State(turtle => (turtle.penUp, Vector.empty))
    case Algebra.PenDown           => State(turtle => (turtle.penDown, Vector.empty))
    case Algebra.SetColor(color)   => State(turtle => (turtle.setColor(color), Vector.empty))
  }

  def animationCreationInterpreter: Interpreter[TurtleState, Canvas.Animation] = {
    case Algebra.Forward(distance) => State { start =>
      val end = start.forward(distance)
      start.pen.state match {
        case Pen.State.Up   => (end, Vector.empty[Canvas.Frame])
        case Pen.State.Down =>
          val (turtle, frame) = Turtle.lineBetween(start, end)

          (turtle, frame.pure[Vector])
      }
    }
    case Algebra.TurnRight(delta)  => State(_.turnRight(delta) -> Vector.empty)
    case Algebra.TurnLeft(delta)   => State(_.turnLeft(delta) -> Vector.empty)
    case Algebra.PenUp             => State(_.penUp -> Vector.empty)
    case Algebra.PenDown           => State(_.penDown -> Vector.empty)
    case Algebra.SetColor(color)   => State(_.setColor(color) -> Vector.empty)
  }
}