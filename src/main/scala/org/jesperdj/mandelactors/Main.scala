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

object Main {
  def main(args: Array[String]) {
    println("MandelActors - Mandelbrot fractal generator using actors")
    println("Copyright (C) 2011  Jesper de Jong")

    println("Initializing")
    val rectangle = new Rectangle(0, 0, Config.imageWidth - 1, Config.imageHeight - 1)

    val p1 = new PalettePoint(0.0f, new Color(0.0f, 0.0f, 0.6f))
    val p2 = new PalettePoint(0.2f, new Color(1.0f, 0.5f, 0.0f))
    val p3 = new PalettePoint(0.4f, new Color(0.7f, 1.0f, 0.5f))
    val p4 = new PalettePoint(0.6f, new Color(0.5f, 0.5f, 0.7f))
    val p5 = new PalettePoint(0.8f, new Color(0.2f, 0.2f, 1.0f))
    val p6 = new PalettePoint(1.0f, Color.White)
    val palette = new Palette(p1, p2, p3, p4, p5, p6)

    val mandelbrot = new Mandelbrot(rectangle, Config.center, Config.scale, Config.maxIterations, palette)

    val sampler = new StratifiedSampler(rectangle, Config.samplesPerPixelX, Config.samplesPerPixelY)
    val image = new Image(rectangle.width, rectangle.height, Config.filter)

    // TODO: The current version does not use actors yet; it just uses the main thread for rendering. Implement this using actors.

    println("Rendering")
    time { for (sample <- sampler.samples) image.add(sample, mandelbrot(sample)) }

    println("Converting image")
    val bufferedImage = image.toBufferedImage

    println("Saving image")
    javax.imageio.ImageIO.write(image.toBufferedImage, "png", new java.io.File("output.png"))

    println("Finished")
  }

  def time[U](block: => U): U = {
    val startTime = java.lang.System.currentTimeMillis
    val result = block
    val endTime = java.lang.System.currentTimeMillis
    println("Time: %d ms" format (endTime - startTime))
    result
  }
}
