package com.peschke.fractals.lsystem

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.eq._
import cats.syntax.traverse._
import cats.syntax.validated._
import com.peschke.fractals.lsystem.LSystem.Element
import com.peschke.fractals.turtle.Turtle.Renderer
import com.peschke.fractals.turtle.{Angle, Distance, Turtle}
import com.typesafe.config.{Config, ConfigFactory}

import scala.collection.JavaConverters._
import scala.util.matching.Regex

case class LSystem(name: String,
                   rules: PartialFunction[Element, Vector[Element]],
                   defaultSeed: Vector[Element],
                   defaultAngle: Angle) {
  def next(seed: Vector[Element]): Vector[Element] =
    seed.flatMap(element => rules.applyOrElse(element, (_: Element).pure[Vector]))
}
object LSystem {
  type ParseResult[A] = ValidatedNel[String, A]
  private val RuleSep: Regex = raw"\s*->\s*".r

  def fromDefinition(name: String, axiomDef: String, transitionDefs: NonEmptyList[String],
                     angle: Angle): ParseResult[LSystem] = {
    transitionDefs.traverse(parseTransition).map { rules =>
      LSystem(
        name = name,
        rules = rules.reduceLeft(_ orElse _),
        defaultSeed = Element.parseSeed(axiomDef),
        defaultAngle = angle
      )
    }
  }

  /**
   * Parse a transition from a string
   *
   * The format is (whitespace and case are ignored):
   * `E->E*`
   */
  def parseTransition(transition: String): ParseResult[PartialFunction[Element, Vector[Element]]] =
    RuleSep.split(transition) match {
      case Array(rawSource, rawSeed) =>
        val validatedSource = rawSource.toVector.filterNot(_.isWhitespace) match {
          case Vector(sourceChar) => Element.parseElement(sourceChar).valid
          case Vector()           => s"Source element was omitted in transition definition: <$transition>".invalidNel
          case _                  => s"Source element must be a single character: <$transition>".invalidNel
        }
        validatedSource.map { source =>
          val seed = Element.parseSeed(rawSeed)
          val pf: PartialFunction[Element, Vector[Element]] = {
            case s if s.symbol === source.symbol => seed
          }
          pf
        }
      case _                         =>
        s"Unable to split transition definition into source and seed: <$transition>".invalidNel
    }

  sealed trait Element {
    def symbol: String
  }
  object Element {
    final case class NoOp(symbol: String) extends Element
    final case class Forward(symbol: String) extends Element
    final case class TurnLeft(symbol: String) extends Element
    final case class TurnRight(symbol: String) extends Element

    def renderer(length: Distance, angle: Angle): Renderer[Element] = {
      case NoOp(_)      => Turtle.Algebra.Forward(Distance(0))
      case Forward(_)   => Turtle.Algebra.Forward(length)
      case TurnLeft(_)  => Turtle.Algebra.TurnLeft(angle)
      case TurnRight(_) => Turtle.Algebra.TurnRight(angle)
    }

    /**
     * Parse an [[Element]] from a character.
     *
     * This is the mapping (case insensitive):
     * [[Forward]] = `F` | `G`
     * [[TurnLeft]] = `+`
     * [[TurnRight]] = `-`
     *
     * Everything else maps to [[NoOp]]
     *
     */
    def parseElement(input: Char): Element = input.toUpper match {
      case s @ ('F' | 'G') => Forward(s.toString)
      case '+'             => TurnLeft("+")
      case '-'             => TurnRight("-")
      case s               => NoOp(s.toString)
    }

    /**
     * Parse a seed from a string.
     *
     * Whitespace is ignored, otherwise the mapping follows the logic in [[parseElement()]]
     */
    def parseSeed(input: String): Vector[Element] =
      input.toVector.filterNot(_.isWhitespace).map(parseElement)
  }

  val values: Vector[LSystem] = {
    def getValue[A](path: String, get: (Config, String) => A)(implicit conf: Config): ParseResult[A] =
      Validated.catchNonFatal(get(conf, path))
        .leftMap(e => NonEmptyList.one(s".$path : load error - ${e.getMessage}"))

    val validateValues: ParseResult[Vector[LSystem]] =
      Validated.catchNonFatal(ConfigFactory.load())
        .leftMap(e => NonEmptyList.one(s" : load error - ${e.getMessage}"))
        .andThen { implicit conf =>
          getValue("lsystems", _ getConfigList _)
            .map(_.asScala.toVector)
        }
        .andThen { elements =>
          elements
            .zipWithIndex
            .traverse {
              case (conf, index) =>
                implicit val c: Config = conf
                val validatedName: ParseResult[String] = getValue("name", _ getString _)
                val validatedAxiomDef: ParseResult[String] = getValue("axiom", _ getString _)
                val validatedTransitionDef: ParseResult[NonEmptyList[String]] =
                  getValue("rules", _ getStringList _)
                    .andThen { asList =>
                      Validated.fromOption(
                        NonEmptyList.fromList(asList.asScala.toList),
                        NonEmptyList.one("rules : cannot be empty")
                      )
                    }
                val validatedAngle: ParseResult[Angle] =
                  getValue("angle.degrees", _ getDouble _).map(Angle.deg)
                (validatedName product
                 validatedAxiomDef product
                 validatedTransitionDef product
                 validatedAngle)
                  .andThen {
                    case (((name, axiom), rules), angle) =>
                      LSystem.fromDefinition(
                        name = name,
                        axiomDef = axiom,
                        angle = angle,
                        transitionDefs = rules
                      )
                  }
                  .leftMap {
                    _.map(error => s".lsystems[$index]$error")
                  }
            }
        }
        .leftMap(_.map(error => s"_$error"))

    validateValues.valueOr { errors =>
      throw new IllegalArgumentException(
        ("Unable to load L-System definitions" :: errors.toList).mkString("\n")
      )
    }
  }
}
