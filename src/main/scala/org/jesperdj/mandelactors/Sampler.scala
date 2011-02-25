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

import scala.collection.immutable.Traversable

class Sample (val x: Float, val y: Float)

trait Sampler {
  val rectangle: Rectangle
  val samplesPerPixel: Int

  val samples: Traversable[Sample]
}

class StratifiedSampler (val rectangle: Rectangle, samplesPerPixelX: Int, samplesPerPixelY: Int) extends Sampler {
  val samplesPerPixel = samplesPerPixelX * samplesPerPixelY

  val samples: Traversable[Sample] = new Traversable[Sample] {
    def foreach[U](f: Sample => U): Unit =
      for (y <- rectangle.top to rectangle.bottom; x <- rectangle.left to rectangle.right) { generateSamples(x, y) foreach f }

    override val size = rectangle.width * rectangle.height * samplesPerPixelX * samplesPerPixelY

    private val random = new scala.util.Random

    // Generate samples for one pixel
    private def generateSamples(x: Int, y: Int): Traversable[Sample] =
      for (sy <- 0 until samplesPerPixelY; sx <- 0 until samplesPerPixelX) yield
        new Sample(x.toFloat + ((sx.toFloat + random.nextFloat) / samplesPerPixelX), y.toFloat + ((sy.toFloat + random.nextFloat) / samplesPerPixelY))
  }
}
