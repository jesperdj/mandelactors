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

trait Renderer {
  def render(sampler: Sampler, compute: (Sample => Color), image: Image)
}

object SingleThreadRenderer extends Renderer {
  override def render(sampler: Sampler, compute: (Sample => Color), image: Image) {
    for (sample <- sampler.samples) image.add(sample, compute(sample))
  }

  override def toString = "SingleThreadRenderer"
}

object ActorsRenderer extends Renderer {
  override def render(sampler: Sampler, compute: (Sample => Color), image: Image) {
    // TODO: Implement ActorsRenderer
    throw new UnsupportedOperationException("Not yet implemented")
  }

  override def toString = "ActorsRenderer"
}
