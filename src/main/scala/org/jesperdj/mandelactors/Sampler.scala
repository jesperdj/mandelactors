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

import scala.collection.immutable.Traversable

case class Sample (x: Float, y: Float)

trait SampleBatch extends Traversable[Sample]

trait Sampler {
  val rectangle: Rectangle
  val samplesPerPixel: Int

  val batches: Traversable[SampleBatch]
}

class StratifiedSampler (val rectangle: Rectangle, samplesPerPixelX: Int, samplesPerPixelY: Int, samplesPerBatch: Int, jitter: Boolean) extends Sampler {
  val samplesPerPixel = samplesPerPixelX * samplesPerPixelY

  val batches = new Traversable[SampleBatch] {
    private class SampleBatchImpl (data: Array[Float]) extends SampleBatch {
      def foreach[U](f: Sample => U): Unit = for (i <- 0 until data.length by 2) f(new Sample(data(i), data(i + 1)))
      override def size = data.length / 2
    }

    def foreach[U](f: SampleBatch => U): Unit = {
      // Number of samples left to generate
      var count = rectangle.width * rectangle.height * samplesPerPixel

      // Indices of the next sample to generate: (px, py) = pixel, (sx, sy) = sample for the current pixel
      var px = rectangle.left; var py = rectangle.top
      var sx = 0; var sy = 0

      val random = new scala.util.Random

      0 to size foreach { _ =>
        val batchSize = math.min(samplesPerBatch, count)
        val data = new Array[Float](2 * batchSize)

        for (i <- 0 until data.length by 2) {
          // Generate a sample
          val (jx, jy) = if (jitter) (random.nextFloat, random.nextFloat) else (0.5f, 0.5f)
          data(i) = px + ((sx + jx) / samplesPerPixelX)
          data(i + 1) = py + ((sy + jy) / samplesPerPixelY)

          // Move indices to the next sample
          sx += 1
          if (sx >= samplesPerPixelX) {
            sx = 0; sy += 1
            if (sy >= samplesPerPixelY) {
              sy = 0; px += 1
              if (px > rectangle.right) { px = rectangle.left; py += 1 }
            }
          }
        }

        f(new SampleBatchImpl(data))

        count -= batchSize
      }
    }

    override val size = ((rectangle.width * rectangle.height * samplesPerPixel) / samplesPerBatch.toFloat).ceil.toInt
  }

  override def toString = "StratifiedSampler"
}
