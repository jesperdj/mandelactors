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

  // Image dimensions
  val imageWidth: Int = JavaInteger.parseInt(props.getProperty("image.width", "1050"))
  val imageHeight: Int = JavaInteger.parseInt(props.getProperty("image.height", "600"))

  // Sampler settings
  val samplesPerPixelX: Int = JavaInteger.parseInt(props.getProperty("sampler.samplesPerPixelX", "1"))
  val samplesPerPixelY: Int = JavaInteger.parseInt(props.getProperty("sampler.samplesPerPixelY", "1"))
  val samplesPerBatch: Int = JavaInteger.parseInt(props.getProperty("sampler.samplesPerBatch", "4096"))
  val jitter: Boolean = JavaBoolean.parseBoolean(props.getProperty("sampler.jitter", "true"))

  // Reconstruction filter
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

  // Renderer
  def renderer: Renderer = {
    props.getProperty("renderer.name", "SingleThread") match {
      case "SingleThread" => SingleThreadRenderer
      case "EventActors" => EventActorsRenderer
      case "ThreadActors" => ThreadActorsRenderer
      case "Threads" => ThreadsRenderer
      case s => println("Invalid renderer specified: %s - using SingleThreadRenderer instead" format s.toString); SingleThreadRenderer
    }
  }

  // Number of actors (for thread-based actor renderer)
  val rendererActorCount: Int = JavaInteger.parseInt(
    props.getProperty("renderer.actorCount", (2 * Runtime.getRuntime.availableProcessors).toString))

  // Number of threads (for ThreadsRenderer)
  val rendererThreadCount: Int = JavaInteger.parseInt(
    props.getProperty("renderer.threadCount", (2 * Runtime.getRuntime.availableProcessors).toString))

  // TODO: Palette configuration

  // Center point on the complex plane
  val center: Complex = Complex(
    JavaDouble.parseDouble(props.getProperty("mandelbrot.center.re", "-0.75")),
    JavaDouble.parseDouble(props.getProperty("mandelbrot.center.im", "0.0"))
  )

  // Zoom scale
  val scale: Double = JavaDouble.parseDouble(props.getProperty("mandelbrot.scale", "1.75"))

  // Maximum number of iterations
  val maxIterations: Int = JavaInteger.parseInt(props.getProperty("mandelbrot.maxIterations", "100"))
}
