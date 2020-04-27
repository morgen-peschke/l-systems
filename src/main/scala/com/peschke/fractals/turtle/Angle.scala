package com.peschke.fractals.turtle

sealed abstract class Angle extends Serializable {
  def degrees: Double

  def radians: Double

  def +(other: Angle): Angle

  def -(other: Angle): Angle

  override def toString: String = "Angle(%3.4f deg|%2.4f rad)".format(degrees, radians)
}
object Angle {
  private final val MaxDegrees: Double = 360.0d
  private final val MaxRadians: Double = Math.PI * 2

  class Degrees private[Angle](val degrees: Double) extends Angle {
    override def radians: Double = (degrees * (Math.PI / 180.0)) % MaxRadians

    override def +(other: Angle): Angle = other match {
      case Degrees(d) => Angle.deg(degrees + d)
      case Radians(r) => Angle.rad(radians + r)
    }

    override def -(other: Angle): Angle = other match {
      case Degrees(d) => Angle.deg(degrees - d)
      case Radians(r) => Angle.rad(radians - r)
    }
  }
  object Degrees {
    def unapply(d: Degrees): Some[Double] = Some(d.degrees)
  }

  class Radians private[Angle](val radians: Double) extends Angle {
    override def degrees: Double = (radians * (180.0d / Math.PI)) % MaxDegrees

    override def +(other: Angle): Angle = Angle.rad(radians + other.radians)

    override def -(other: Angle): Angle = Angle.rad(radians - other.radians)
  }
  object Radians {
    def unapply(r: Radians): Some[Double] = Some(r.radians)
  }

  def deg(degrees: Double): Angle = new Degrees(degrees % MaxDegrees)

  def rad(radians: Double): Angle = new Radians(radians % MaxRadians)
}