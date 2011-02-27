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

object Config {
  import java.lang.{ Boolean => JavaBoolean, Integer => JavaInteger, Double => JavaDouble }

  private val props = {
    val properties = new java.util.Properties
    val file = new java.io.File("mandelactors.properties")
    if (file.exists) {
      properties.load(new java.io.FileReader(file))
    }
    properties
  }

  val imageWidth: Int = JavaInteger.parseInt(props.getProperty("image.width", "1050"))
  val imageHeight: Int = JavaInteger.parseInt(props.getProperty("image.height", "600"))

  val samplesPerPixelX: Int = JavaInteger.parseInt(props.getProperty("sampler.samplesPerPixelX", "1"))
  val samplesPerPixelY: Int = JavaInteger.parseInt(props.getProperty("sampler.samplesPerPixelY", "1"))
  val jitter: Boolean = JavaBoolean.parseBoolean(props.getProperty("sampler.jitter", "true"))

  def filter: Filter = {
    props.getProperty("filter.name", "Box") match {
      case "Box" => new BoxFilter
      case "Triangle" => new TriangleFilter
      case "Gaussian" => new GaussianFilter
      case "Mitchell" => new MitchellFilter
      case "LanczosSinc" => new LanczosSincFilter
      case s => println("Invalid filter specified: %s - using BoxFilter instead" format s.toString); new BoxFilter
    }
  }

  def renderer: Renderer = {
    props.getProperty("renderer.name", "SingleThread") match {
      case "SingleThread" => SingleThreadRenderer
      case "Actors" => ActorsRenderer
      case s => println("Invalid renderer specified: %s - using SingleThreadRenderer instead" format s.toString); SingleThreadRenderer
    }
  }

  val actorsRendererBatchSize: Int = JavaInteger.parseInt(props.getProperty("renderer.batchSize", "1024"))

  // TODO: Palette configuration

  val center: Complex = Complex(
    JavaDouble.parseDouble(props.getProperty("mandelbrot.center.re", "-0.75")),
    JavaDouble.parseDouble(props.getProperty("mandelbrot.center.im", "0.0"))
  )

  val scale: Double = JavaDouble.parseDouble(props.getProperty("mandelbrot.scale", "1.75"))
  val maxIterations: Int = JavaInteger.parseInt(props.getProperty("mandelbrot.maxIterations", "1000"))
}
