package com.peschke.fractals.lsystem

import cats.instances.vector._
import cats.instances.string._
import cats.syntax.eq._
import cats.syntax.applicative._
import com.peschke.fractals.lsystem.LSystem.Element
import com.peschke.fractals.turtle.{Angle, Distance, Turtle}
import com.peschke.fractals.turtle.Turtle.Renderer

case class LSystem(rules: PartialFunction[Element, Vector[Element]],
                     defaultSeed: Vector[Element]) {
  def next(seed: Vector[Element]): Vector[Element] =
    seed.flatMap(element => rules.applyOrElse(element, (_: Element).pure[Vector]))
}
object LSystem {

  sealed trait Element {
    def symbol: String
  }
  object Element {
    final case class NoOp(symbol: String) extends Element
    final case class Forward(symbol: String, distance: Distance) extends Element
    final case class TurnLeft(symbol: String, angle: Angle) extends Element
    final case class TurnRight(symbol: String, angle: Angle) extends Element

    val renderer: Renderer[Element] = {
      case NoOp(_)             => Turtle.Algebra.Forward(Distance(0))
      case Forward(_, length)  => Turtle.Algebra.Forward(length)
      case TurnLeft(_, angle)  => Turtle.Algebra.TurnLeft(angle)
      case TurnRight(_, angle) => Turtle.Algebra.TurnRight(angle)
    }

    object dsl {
      class DSLNoOp(symbol: String) {
        def make: Element = NoOp(symbol)

        def unapply(e: Element): Boolean = e match {
          case NoOp(s) => s === symbol
          case _       => false
        }
      }
      class DSLForward(symbol: String) {
        def make(length: Distance): Element = Forward(symbol, length)

        def unapply(e: Element): Boolean = e match {
          case Forward(s, _) => s === symbol
          case _             => false
        }
      }
      class DSLLeft(symbol: String) {
        def make(angle: Angle): Element = TurnLeft(symbol, angle)

        def unapply(e: Element): Boolean = e match {
          case TurnLeft(s, _) => s === symbol
          case _              => false
        }
      }
      class DSLRight(symbol: String) {
        def make(angle: Angle): Element = TurnRight(symbol, angle)

        def unapply(e: Element): Boolean = e match {
          case TurnRight(s, _) => s === symbol
          case _               => false
        }
      }

      object X extends DSLNoOp("X")
      val x: Element = X.make

      object Y extends DSLNoOp("Y")
      val y: Element = Y.make

      object F extends DSLForward("F")
      def f(implicit L: Distance): Element = F.make(L)

      object G extends DSLForward("G")
      def g(implicit L: Distance): Element = G.make(L)

      object :+: extends DSLLeft("+")
      def + (implicit angle: Angle): Element = :+:.make(angle)

      object :-: extends DSLRight("-")
      def - (implicit angle: Angle): Element = :-:.make(angle)
    }
  }
}
