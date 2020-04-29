package com.peschke.fractals.lsystem

import cats.instances.vector._
import cats.syntax.applicative._
import com.peschke.fractals.lsystem.LSystem.Element
import com.peschke.fractals.lsystem.LSystem.Element.{Forward, NoOp, TurnLeft, TurnRight}
import com.peschke.fractals.turtle.{Angle, Turtle}
import com.peschke.fractals.turtle.Turtle.Renderer

trait LSystem {
  def rules: PartialFunction[Element, Vector[Element]]

  def defaultSeed: Vector[Element]

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
      case NoOp(_)             => Turtle.Algebra.Forward(0)
      case Forward(_, length)  => Turtle.Algebra.Forward(length)
      case TurnLeft(_, angle)  => Turtle.Algebra.TurnLeft(angle)
      case TurnRight(_, angle) => Turtle.Algebra.TurnRight(angle)
    }
  }

  abstract class LSystemDSL(length: Double, angle: Angle) {
    val F: Forward = Forward("F", length)
    val plus: TurnLeft = TurnLeft("+", angle)
    val minus: TurnRight = TurnRight("-", angle)
  }

  final case class KochCurve(length: Double, angle: Angle = Angle.deg(90))
    extends LSystemDSL(length, angle)
      with LSystem {
    val rules: PartialFunction[Element, Vector[Element]] = {
      case F => Vector(F, plus, F, minus, F, minus, F, plus, F)
    }
    val defaultSeed: Vector[Element] = F.pure[Vector]
  }

  final case class KochSnowflake(length: Double, angle: Angle = Angle.deg(60))
    extends LSystemDSL(length, angle)
      with LSystem {
    val rules: PartialFunction[Element, Vector[Element]] = {
      case F => Vector(F, plus, F, minus, minus, F, plus, F)
    }
    val defaultSeed: Vector[Element] = F.pure[Vector]
  }

  final case class DragonCurve(length: Double, angle: Angle = Angle.deg(90)) extends LSystemDSL(length, angle) with LSystem {
    private final val X = NoOp("X")
    private final val Y = NoOp("Y")
    val rules: PartialFunction[Element, Vector[Element]] = {
      case X => Vector(X, plus, Y, F, plus)
      case Y => Vector(minus, F, X, minus, Y)
    }
    val defaultSeed: Vector[Element] = Vector(F, X)
  }

  final case class SierpinskiTriangle(length: Double, angle: Angle = Angle.deg(120))
    extends LSystemDSL(length, angle)
      with LSystem {
    private final val G = Forward("G", length)
    val rules: PartialFunction[Element, Vector[Element]] = {
      case F => Vector(F, minus, G, plus, F, plus, G, minus, F)
      case G => Vector(G, G)
    }
    val defaultSeed: Vector[Element] = Vector(TurnRight("", Angle.deg(60)), F, minus, G, minus, G)
  }

  final case class SierpinskiArrowHead(length: Double, angle: Angle = Angle.deg(60))
    extends LSystemDSL(length, angle)
      with LSystem {
    private final val A = Forward("A", length)
    private final val B = Forward("B", length)
    val rules: PartialFunction[Element, Vector[Element]] = {
      case A => Vector(B, minus, A, minus, B)
      case B => Vector(A, plus, B, plus, A)
    }
    val defaultSeed: Vector[Element] = Vector(A)
  }
}
