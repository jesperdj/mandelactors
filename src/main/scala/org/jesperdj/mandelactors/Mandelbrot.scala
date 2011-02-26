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

class Complex(val re: Double, val im: Double) {
  def +(c: Complex) = new Complex(re + c.re, im + c.im)
  def *(c: Complex) = new Complex(re * c.re - im * c.im, im * c.re + re * c.im)
  def modulusSquared = re * re + im * im
  def modulus = math.sqrt(modulusSquared)

  override def toString = "(%.12g, %.12g)" format (re, im)
}

object Complex {
  val Zero = new Complex(0.0, 0.0)
  def apply(re: Double, im: Double) = new Complex(re, im)
}

class Mandelbrot (rectangle: Rectangle, center: Complex, scale: Double, maxIterations: Int, palette: Palette) extends (Sample => Color) {
  private val lg2 = math.log(2.0)
  private def log2(value: Double) = math.log(value) / lg2

  private val rectWidth = rectangle.width.toDouble
  private val rectHeight = rectangle.height.toDouble

  private val (minC, maxC) = {
    val ratio = rectWidth / rectHeight
    (Complex(center.re - scale, center.im - scale / ratio), Complex(center.re + scale, center.im + scale / ratio))
  }

  private val planeWidth = maxC.re - minC.re
  private val planeHeight = maxC.im - minC.im

  def apply(sample: Sample): Color = {
    // Map sample coordinates to imaginary plane coordinates
    val c = Complex(minC.re + (planeWidth * (sample.x - rectangle.left) / rectWidth), maxC.im - (planeHeight * (sample.y - rectangle.top) / rectHeight))

    var z = Complex.Zero
    var i = 0

    while (z.modulusSquared <= 4.0 && i < maxIterations) {
      z = z * z + c
      i += 1
    }

    // Use normalized iteration count for smooth coloring
    if (i < maxIterations) palette((i.toFloat - log2(log2(z.modulus)).toFloat) / maxIterations.toFloat) else Color.Black
  }
}
