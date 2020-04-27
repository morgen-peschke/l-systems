package com.peschke.fractals.lsystem

import cats.instances.vector._
import cats.syntax.applicative._
import com.peschke.fractals.lsystem.LSystem.Element
import com.peschke.fractals.lsystem.LSystem.Element.{Forward, NoOp, TurnLeft, TurnRight}
import com.peschke.fractals.turtle.{Angle, Turtle}
import com.peschke.fractals.turtle.Turtle.Renderer

case class LSystem(rules: PartialFunction[Element, Vector[Element]],
                   defaultSeed: Vector[Element],
                  ) {

  def next(seed: Vector[Element]): Vector[Element] =
    seed.flatMap(element => rules.applyOrElse(element, (_: Element).pure[Vector]))
}
object LSystem {
  sealed trait Element {
    def symbol: String
  }
  object Element {
    final case class NoOp(symbol: String) extends Element
    final case class Forward(symbol: String, distance: Double) extends Element
    final case class TurnLeft(symbol: String, angle: Angle) extends Element
    final case class TurnRight(symbol: String, angle: Angle) extends Element

    val renderer: Renderer[Element] = {
      case NoOp(_)            => Turtle.Algebra.Forward(0)
      case Forward(_, length) => Turtle.Algebra.Forward(length)
      case TurnLeft(_, angle) => Turtle.Algebra.TurnLeft(angle)
      case TurnRight(_, angle) => Turtle.Algebra.TurnRight(angle)
    }
  }

  def kochCurve(length: Double, angle: Angle = Angle.deg(90)): LSystem = {
    val F = Forward("F", length)
    val plus = TurnLeft("+", angle)
    val minus = TurnRight("-", angle)
    LSystem(
      rules = {
        case F => Vector(F, plus, F, minus, F, minus, F, plus, F)
      },
      defaultSeed = F.pure[Vector]
    )
  }

  def kochSnowflake(length: Double, angle: Angle = Angle.deg(60)): LSystem = {
    val F = Forward("F", length)
    val plus = TurnLeft("+", angle)
    val minus = TurnRight("-", angle)
    LSystem(
      rules = {
        case F => Vector(F, plus, F, minus, minus, F, plus, F)
      },
      defaultSeed = F.pure[Vector]
    )
  }

  def dragonCurve(length: Double, angle: Angle = Angle.deg(90)): LSystem = {
    val F = Forward("F", length)
    val X = NoOp("X")
    val Y = NoOp("Y")
    val plus = TurnLeft("+", angle)
    val minus = TurnRight("-", angle)
    LSystem(
      rules = {
        case X => Vector(X, plus, Y, F, plus)
        case Y => Vector(minus, F, X, minus, Y)
      },
      defaultSeed = Vector(F, X)
    )
  }

  def sierpinskiTriangle(length: Double, angle: Angle = Angle.deg(120)): LSystem = {
    val F = Forward("F", length)
    val G = Forward("G", length)
    val plus = TurnLeft("+", angle)
    val minus = TurnRight("-", angle)
    LSystem(
      rules = {
        case F => Vector(F, minus, G, plus, F, plus, G, minus, F)
        case G => Vector(G, G)
      },
      defaultSeed = Vector(TurnRight("", Angle.deg(60)), F, minus, G, minus, G)
    )
  }

  def sierpinskiArrowHead(length: Double, angle: Angle = Angle.deg(60)): LSystem = {
      val A = Forward("A", length)
      val B = Forward("B", length)
      val plus = TurnLeft("+", angle)
      val minus = TurnRight("-", angle)
      LSystem(
        rules = {
          case A => Vector(B,minus,A,minus,B)
          case B => Vector(A,plus,B,plus,A)
        },
        defaultSeed = Vector(A)
      )
    }
}
