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

  private val numberOfBatches = ((rectangle.width * rectangle.height * samplesPerPixel) / samplesPerBatch.toFloat).ceil.toInt

  val batches = new Traversable[SampleBatch] {
    private class SampleBatchImpl (batchIndex: Int) extends SampleBatch {
      def foreach[U](f: Sample => U): Unit = {
        val sampleIndex = batchIndex * samplesPerBatch

        val samplesPerPY = rectangle.width * samplesPerPixel
        val samplesPerSY = (sampleIndex % samplesPerPY) % samplesPerPixel

        var py = rectangle.top + sampleIndex / samplesPerPY
        var px = rectangle.left + (sampleIndex % samplesPerPY) / samplesPerPixel
        var sy = samplesPerSY / samplesPerPixelX
        var sx = samplesPerSY % samplesPerPixelX

        val random = new scala.util.Random

        0 until size foreach { _ =>
          // Generate a sample
          val (jx, jy) = if (jitter) (random.nextFloat, random.nextFloat) else (0.5f, 0.5f)
          f(new Sample(px + ((sx + jx) / samplesPerPixelX), py + ((sy + jy) / samplesPerPixelY)))

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
      }

      override def size = if (batchIndex < numberOfBatches - 1) samplesPerBatch else {
        // The last batch contains the remaining samples (can be less than samplesPerBatch)
        rectangle.width * rectangle.height * samplesPerPixel - (numberOfBatches - 1) * samplesPerBatch
      }
    }

    def foreach[U](f: SampleBatch => U): Unit = for (batchIndex <- 0 until size) f(new SampleBatchImpl(batchIndex))

    override val size = numberOfBatches
  }

  override def toString = "StratifiedSampler"
}
