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
  import java.lang.{ Double => JavaDouble, Integer => JavaInteger }

  private val props = {
    val properties = new java.util.Properties
    val file = new java.io.File("mandelactors.properties")
    if (file.exists) {
      properties.load(new java.io.FileReader(file))
    }
    properties
  }

  val imageWidth = JavaInteger.parseInt(props.getProperty("image.width", "1050"))
  val imageHeight = JavaInteger.parseInt(props.getProperty("image.height", "600"))

  val samplesPerPixelX = JavaInteger.parseInt(props.getProperty("samples.x", "1"))
  val samplesPerPixelY = JavaInteger.parseInt(props.getProperty("samples.y", "1"))

  def filter: Filter = {
    props.getProperty("filter", "Box") match {
      case "Box" => new BoxFilter
      case "Triangle" => new TriangleFilter
      case "Gaussian" => new GaussianFilter
      case "Mitchell" => new MitchellFilter
      case "LanczosSinc" => new LanczosSincFilter
      case _ => new BoxFilter
    }
  }

  // TODO: Palette configuration

  val center = Complex(JavaDouble.parseDouble(props.getProperty("center.re", "-0.75")), JavaDouble.parseDouble(props.getProperty("center.im", "0.0")))
  val scale = JavaDouble.parseDouble(props.getProperty("scale", "1.75"))
  val maxIterations = JavaInteger.parseInt(props.getProperty("maxIterations", "1000"))
}
