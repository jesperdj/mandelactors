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

class Rectangle (val left: Int, val top: Int, val right: Int, val bottom: Int) {
  val width = right - left + 1
  val height = bottom - top + 1
}

object Rectangle {
  def apply(left: Int, top: Int, right: Int, bottom: Int) = new Rectangle(left, top, right, bottom)
  def apply(width: Int, height: Int) = new Rectangle(0, 0, width - 1, height - 1)
}

class Raster[T : ClassManifest] (val rectangle: Rectangle) {
  private val data = new Array[T](rectangle.width * rectangle.height)

  private def index(x: Int, y: Int): Int = (x - rectangle.left) + (y - rectangle.top) * rectangle.width

  def apply(x: Int, y: Int): T = data(index(x, y))
  def update(x: Int, y: Int, value: T): Unit = data(index(x, y)) = value
}
