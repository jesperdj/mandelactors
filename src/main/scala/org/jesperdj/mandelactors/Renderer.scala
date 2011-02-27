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
  def render(sampler: Sampler, compute: Sample => Color, image: Image)
}

object SingleThreadRenderer extends Renderer {
  override def render(sampler: Sampler, compute: Sample => Color, image: Image) {
    for (sample <- sampler.samples) image.add(sample, compute(sample))
  }

  override def toString = "SingleThreadRenderer"
}

// Renderer that uses event-based actors
object EventActorsRenderer extends Renderer {
  import scala.actors.Actor._
  import java.util.concurrent.CountDownLatch

  private case class Batch(samples: Traversable[Sample])

  override def render(sampler: Sampler, compute: Sample => Color, image: Image) {
    val batchSize = Config.actorsRendererBatchSize
    println("- Batch size: " + batchSize)

    val countDownLatch = new CountDownLatch(sampler.samples.size)

    println("- Starting actors")
    var samples = sampler.samples
    while (samples.nonEmpty) {
      // Create an event-driven actor to do the computation for a batch of samples
      val computeActor = actor {
        react {
          case b: Batch => for (s <- b.samples) { image.add(s, compute(s)); countDownLatch.countDown }
        }
      }

      // Get a batch of samples and send it to the actor
      val (batch, rest) = samples.splitAt(batchSize)
      computeActor ! Batch(batch)
      samples = rest
    }

    // Wait for all the actors to finish
    println("- Waiting for actors to finish")
    countDownLatch.await
  }

  override def toString = "EventActorsRenderer"
}

// Renderer that uses thread-based actors
object ThreadActorsRenderer extends Renderer {
  import scala.actors.Actor
  import java.util.concurrent.CountDownLatch

  private case class Batch(samples: Traversable[Sample])

  private class ComputeActor (compute: Sample => Color, image: Image, countDownLatch: CountDownLatch) extends Actor {
    def act() {
      loop {
        receive {
          case b: Batch => for (s <- b.samples) { image.add(s, compute(s)); countDownLatch.countDown }
          case 'Exit => exit
        }
      }
    }
  }

  override def render(sampler: Sampler, compute: Sample => Color, image: Image) {
    val batchSize = Config.actorsRendererBatchSize
    println("- Batch size: " + batchSize)

    val countDownLatch = new CountDownLatch(sampler.samples.size)

    val actorCount = Config.actorsRendererActorCount

    println("- Starting actors; number of actors: " + actorCount)
    val computeActors = new Array[Actor](actorCount)
    for (i <- 0 until actorCount) computeActors(i) = new ComputeActor(compute, image, countDownLatch).start

    println("- Sending messages")
    var i = 0
    var samples = sampler.samples
    while (samples.nonEmpty) {
      // Select the actor to send the next message to
      val computeActor = computeActors(i % actorCount); i += 1

      // Get a batch of samples and send it to the actor
      val (batch, rest) = samples.splitAt(batchSize)
      computeActor ! Batch(batch)
      samples = rest
    }

    // Wait for all the actors to finish
    println("- Waiting for actors to finish")
    countDownLatch.await

    // Stop the actors
    println("- Stopping actors")
    computeActors foreach { _ ! 'Exit }
  }

  override def toString = "ThreadActorsRenderer"
}
