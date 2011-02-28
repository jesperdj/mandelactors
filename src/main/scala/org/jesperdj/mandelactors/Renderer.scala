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
  def render(sampler: Sampler, compute: Sample => Color, image: Image): Unit
}

object SingleThreadRenderer extends Renderer {
  override def render(sampler: Sampler, compute: Sample => Color, image: Image) {
    for (batch <- sampler.batches; sample <- batch) image.add(sample, compute(sample))
  }

  override def toString = "SingleThreadRenderer"
}

// Renderer that uses event-based actors
object EventActorsRenderer extends Renderer {
  import scala.actors.Actor._
  import java.util.concurrent.CountDownLatch

  override def render(sampler: Sampler, compute: Sample => Color, image: Image) {
    val countDownLatch = new CountDownLatch(sampler.batches.size)

    println("- Starting actors")
    for (batch <- sampler.batches) {
      // Create an event-driven actor to do the computation for a batch of samples
      val computeActor = actor {
        react {
          case b: SampleBatch =>
            for (sample <- b) image.add(sample, compute(sample))
            countDownLatch.countDown
        }
      }

      // Send the batch to the actor
      computeActor ! batch
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

  private class ComputeActor (compute: Sample => Color, image: Image, countDownLatch: CountDownLatch) extends Actor {
    def act() {
      loop {
        receive {
          case batch: SampleBatch =>
            for (sample <- batch) image.add(sample, compute(sample))
            countDownLatch.countDown

          case 'Exit => exit
        }
      }
    }
  }

  override def render(sampler: Sampler, compute: Sample => Color, image: Image) {
    val countDownLatch = new CountDownLatch(sampler.batches.size)

    val actorCount = Config.rendererActorCount

    println("- Starting actors; number of actors: " + actorCount)
    val computeActors = new Array[Actor](actorCount)
    for (i <- 0 until actorCount) computeActors(i) = new ComputeActor(compute, image, countDownLatch).start

    println("- Sending messages")
    var i = 0
    for (batch <- sampler.batches) {
      // Select the actor to send the next message to
      val computeActor = computeActors(i % actorCount); i += 1

      // Send the batch to the actor
      computeActor ! batch
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

// Renderer that uses threads (no actors)
object ThreadsRenderer extends Renderer {
  import java.util.concurrent.{ CountDownLatch, BlockingQueue, LinkedBlockingQueue }

  private val workQueue: BlockingQueue[SampleBatch] = new LinkedBlockingQueue[SampleBatch]()

  private class ComputeThread (compute: Sample => Color, image: Image, countDownLatch: CountDownLatch) extends Thread {
    override def run() {
      try {
        while (true) {
          val batch = workQueue.take
          for (sample <- batch) image.add(sample, compute(sample))
          countDownLatch.countDown
        }
      }
      catch {
        case ex: InterruptedException => // Let the thread stop
      }
    }
  }

  def render(sampler: Sampler, compute: Sample => Color, image: Image) {
    val countDownLatch = new CountDownLatch(sampler.batches.size)

    val threadCount = Config.rendererThreadCount

    println("- Starting threads; number of threads: " + threadCount)
    val computeThreads = new Array[Thread](threadCount)
    for (i <- 0 until threadCount) {
      computeThreads(i) = new ComputeThread(compute, image, countDownLatch)
      computeThreads(i).start
    }

    println("- Submitting batches")
    for (batch <- sampler.batches) workQueue.put(batch)

    // Wait for all the threads to finish
    println("- Waiting for threads to finish")
    countDownLatch.await

    // Stop the threads
    println("- Stopping threads")
    computeThreads foreach { _.interrupt }
  }

  override def toString = "ThreadsRenderer"
}
