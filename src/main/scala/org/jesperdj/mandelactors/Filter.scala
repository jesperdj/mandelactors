/*
 * MandelActors - Mandelbrot fractal generator using actors
 * Copyright (C) 2011  Jesper de Jong
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.jesperdj.mandelactors

// Ideas for sampling and reconstruction are reused from my project ScalaRay - https://github.com/jesperdj/scalaray
// and come from the book Physically Based Rendering - From Theory to Implementation - http://www.pbrt.org/

trait Filter extends ((Float, Float) => Float) {
  val extentX: Float
  val extentY: Float
}

class BoxFilter (val extentX: Float = 0.5f, val extentY: Float = 0.5f) extends Filter {
  def apply(x: Float, y: Float) = 1.0f

  override def toString = "BoxFilter(%.3g, %.3g)" format (extentX, extentY)
}

class TriangleFilter (val extentX: Float = 2.0f, val extentY: Float = 2.0f) extends Filter {
  def apply(x: Float, y: Float) = math.max(0.0f, extentX - x.abs) * math.max(0.0f, extentY - y.abs)

  override def toString = "TriangleFilter(%.3g, %.3g)" format (extentX, extentY)
}

class GaussianFilter (val extentX: Float = 2.0f, val extentY: Float = 2.0f, alpha: Float = 2.0f) extends Filter {
  private val expX = math.exp(-alpha * extentX * extentX).toFloat
  private val expY = math.exp(-alpha * extentY * extentY).toFloat

  private def gaussian(d: Float, exp: Float) = math.max(0.0f, math.exp(-alpha * d * d).toFloat - exp)

  def apply(x: Float, y: Float) = gaussian(x, expX) * gaussian(y, expY)

  override def toString = "GaussianFilter(%.3g, %.3g, %.3g)" format (extentX, extentY, alpha)
}

class MitchellFilter (val extentX: Float = 2.0f, val extentY: Float = 2.0f, b: Float = 1.0f / 3.0f, c: Float = 1.0f / 3.0f) extends Filter {
  private val (p10, p11, p12, p13) = (1.0f - b / 3.0f, 0.0f, -3.0f + 2.0f * b + c, 2.0f - 1.5f * b - c)
  private val (p20, p21, p22, p23) = (4.0f / 3.0f * b + 4.0f * c, -2.0f * b - 8.0f * c, b + 5.0f * c, -b / 6.0f - c)

  private def mitchell(v: Float) = {
    val x = 2.0f * v.abs; if (x <= 1.0f) p10 + p11 * x + p12 * x * x + p13 * x * x * x else p20 + p21 * x + p22 * x * x + p23 * x * x * x
  }

  def apply(x: Float, y: Float) = mitchell(x / extentX) * mitchell(y / extentY)

  override def toString = "MitchellFilter(%.3g, %.3g, %.3g, %.3g)" format (extentX, extentY, b, c)
}

class LanczosSincFilter (val extentX: Float = 4.0f, val extentY: Float = 4.0f, tau: Float = 3.0f) extends Filter {
  private val π = math.Pi.toFloat

  private def lanczosSinc(v: Float) = {
    val x = v.abs
    if (x < 1e-6f) 1.0f else if (x > 1.0f) 0.0f else {
      val w = π * x; val wt = w * tau; (math.sin(wt).toFloat / wt) * (math.sin(w).toFloat / w)
    }
  }

  def apply(x: Float, y: Float) = lanczosSinc(x / extentX) * lanczosSinc(y / extentY)

  override def toString = "LanczosSincFilter(%.3g, %.3g, %.3g)" format (extentX, extentY, tau)
}
