package com.peschke.fractals.turtle

class Distance(val value: Double) extends AnyVal
object Distance {
  def apply(v: Double): Distance = new Distance(v)
}