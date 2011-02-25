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

class Color (val red: Float, val green: Float, val blue: Float) {
  def +(color: Color) = new Color(red + color.red, green + color.green, blue + color.blue)

  def *(weight: Float): Color = new Color(weight * red, weight * green, weight * blue)
  def /(weight: Float): Color = if (weight == 0.0f) Color.Black else new Color(red / weight, green / weight, blue / weight)

  def +*(color: Color, weight: Float): Color = new Color(red + weight * color.red, green + weight * color.green, blue + weight * color.blue)
}

object Color {
  val Black = new Color(0.0f, 0.0f, 0.0f)
  val White = new Color(1.0f, 1.0f, 1.0f)
}

class PalettePoint (val value: Float, val color: Color)

class Palette (points: PalettePoint*) {
  def apply(value: Float): Color = {
    val (hd, tl) = points span { _.value <= value }
    if (hd.isEmpty) tl.head.color else if (tl.isEmpty) hd.last.color else {
      val left = hd.last
      val right = tl.head

      val weight = (value - left.value) / (right.value - left.value)
      left.color * (1.0f - weight) + right.color * weight
    }
  }
}
