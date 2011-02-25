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

class Pixel (var color: Color = Color.Black)

trait Weight {
  var weight: Float = 0.0f
}

class Image (width: Int, height: Int, filter: Filter) {
  private val rectangle = new Rectangle(0, 0, width - 1, height - 1)

  private val raster = {
    val r = new Raster[Pixel with Weight](rectangle)
    for (y <- rectangle.top to rectangle.bottom; x <- rectangle.left to rectangle.right) r(x, y) = new Pixel with Weight
    r
  }

  def add(sample: Sample, color: Color): Unit = {
    val ix = sample.x - 0.5f
    val iy = sample.y - 0.5f

    val minX = math.max((ix - filter.extentX).ceil.toInt, rectangle.left)
    val maxX = math.min((ix + filter.extentX).floor.toInt, rectangle.right)
    val minY = math.max((iy - filter.extentY).ceil.toInt, rectangle.top)
    val maxY = math.min((iy + filter.extentY).floor.toInt, rectangle.bottom)

    for (y <- minY to maxY; x <- minX to maxX) {
      val pixel = raster(x, y)
      val weight = filter(x - ix, y - iy)

      pixel.synchronized {
        pixel.color +*= (color, weight)
        pixel.weight += weight
      }
    }
  }

  def toBufferedImage: java.awt.image.BufferedImage = {
    def toByte(value: Float): Int = if (value < 0.0f) 0 else if (value > 1.0f) 255 else (value * 255.0f).toInt

    val img = new java.awt.image.BufferedImage(width, height, java.awt.image.BufferedImage.TYPE_INT_RGB)

    for (y <- rectangle.top to rectangle.bottom; x <- rectangle.left to rectangle.right) {
      val px = x - rectangle.left
      val py = y - rectangle.top

      val pixel = raster(px, py)
      val color = if (pixel.weight != 0.0f) pixel.color / pixel.weight else Color.Black

      img.setRGB(px, py, toByte(color.red) << 16 | toByte(color.green) << 8 | toByte(color.blue))
    }

    img
  }
}
