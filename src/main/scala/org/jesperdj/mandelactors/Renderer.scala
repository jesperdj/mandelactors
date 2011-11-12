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
  def render(sampler: Sampler, compute: Sample => Color, pixelBuffer: PixelBuffer): Unit
}

object SingleThreadRenderer extends Renderer {
  override def render(sampler: Sampler, compute: Sample => Color, pixelBuffer: PixelBuffer) {
    for (batch <- sampler.batches; sample <- batch) pixelBuffer.add(sample, compute(sample))
  }

  override def toString = "SingleThreadRenderer"
}

object ParallelCollectionsRenderer extends Renderer {
  override def render(sampler: Sampler, compute: Sample => Color, pixelBuffer: PixelBuffer) {
    for (batch <- sampler.batches.par; sample <- batch) pixelBuffer.add(sample, compute(sample))
  }

  override def toString = "ParallelCollectionsRenderer"
}

// Renderer that uses event-based actors
object EventActorsRenderer extends Renderer {
  import scala.actors.Actor._
  import java.util.concurrent.CountDownLatch

  override def render(sampler: Sampler, compute: Sample => Color, pixelBuffer: PixelBuffer) {
    val countDownLatch = new CountDownLatch(sampler.batches.size)

    println("- Starting actors")
    for (batch <- sampler.batches) {
      // Create an event-driven actor to do the computation for a batch of samples
      val computeActor = actor {
        react {
          case b: SampleBatch =>
            for (sample <- b) pixelBuffer.add(sample, compute(sample))
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

  private class ComputeActor (compute: Sample => Color, pixelBuffer: PixelBuffer, countDownLatch: CountDownLatch) extends Actor {
    def act() {
      loop {
        receive {
          case batch: SampleBatch =>
            for (sample <- batch) pixelBuffer.add(sample, compute(sample))
            countDownLatch.countDown

          case 'Exit => exit
        }
      }
    }
  }

  override def render(sampler: Sampler, compute: Sample => Color, pixelBuffer: PixelBuffer) {
    val countDownLatch = new CountDownLatch(sampler.batches.size)

    val actorCount = Config.rendererActorCount

    println("- Starting actors; number of actors: " + actorCount)
    val computeActors = new Array[Actor](actorCount)
    for (i <- 0 until actorCount) computeActors(i) = new ComputeActor(compute, pixelBuffer, countDownLatch).start

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

  private class ComputeThread (compute: Sample => Color, pixelBuffer: PixelBuffer, countDownLatch: CountDownLatch) extends Thread {
    override def run() {
      try {
        while (true) {
          val batch = workQueue.take
          for (sample <- batch) pixelBuffer.add(sample, compute(sample))
          countDownLatch.countDown
        }
      }
      catch {
        case ex: InterruptedException => // Let the thread stop
      }
    }
  }

  def render(sampler: Sampler, compute: Sample => Color, pixelBuffer: PixelBuffer) {
    val countDownLatch = new CountDownLatch(sampler.batches.size)

    val threadCount = Config.rendererThreadCount

    println("- Starting threads; number of threads: " + threadCount)
    val computeThreads = new Array[Thread](threadCount)
    for (i <- 0 until threadCount) {
      computeThreads(i) = new ComputeThread(compute, pixelBuffer, countDownLatch)
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

// The code below was contributed by Andre Kovacs

// Renderer that uses event-based remote actors with futures
object EventFuturesRemoteActorsRenderer extends Renderer {
  import scala.actors.Actor._
  import scala.actors.AbstractActor
  import scala.actors.remote.RemoteActor.{select}
  import scala.actors.remote.Node
  import scala.collection.mutable.HashMap

  override def render(sampler: Sampler, compute: Sample => Color, pixelBuffer: PixelBuffer) {
    println("- Starting server")
    val serverStart = new Server
    serverStart.start()

    val server = select(Node("localhost", 9000), 'server)
    println("- Connected to server")

    val actorCount = Config.rendererActorCount

    println("- Starting actors; number of actors: " + actorCount)
    val computeActors = new Array[AbstractActor](actorCount)
    for (i <- 0 until actorCount) {
      server ! Start(classOf[EventRemoteActor])

      // Create an event-driven actor to do the computation for a batch of samples
      val computeActor = select(Node("localhost", 9000), 'mandelbench)
      println("- Connected to actor")

      computeActors(i) = computeActor
    }

    var i = 0
    val dataFutures = for (batch <- sampler.batches) yield {
      // Select the actor to send the next message to
      val computeActor = computeActors(i % actorCount); i += 1

      // Send the batch to the actor
      computeActor !! Render(batch, compute)
    }

    for (future <- dataFutures) {
      future.inputChannel.receive {
        case pixelMap: HashMap[Sample, Color] =>
          val pixelBufferActor = actor {
            react {
              case pixelMap: HashMap[Sample,Color] =>
                for (sample <- pixelMap.keys) {
                  val color: Option[Color] = pixelMap.get(sample)
                  pixelBuffer.add(sample, color.get)
                }
            }
          }

          // Send the pixelMap to the actor
          pixelBufferActor ! pixelMap

        case _ => println("- Error")
      }
    }

    for (i <- 0 until actorCount) computeActors(i) ! Stop

    server ! Stop

    // Wait for all the actors to finish
    println("- Waiting for actors to finish")
  }

  override def toString = "EventFuturesRemoteActorsRenderer"
}

case class Render(b: SampleBatch, compute: Sample => Color)

class EventRemoteActor extends scala.actors.Actor with Serializable {
  import scala.actors.remote.RemoteActor.{alive, register}
  import scala.actors.remote.RemoteActor
  import scala.collection.mutable.HashMap

  def act() {
    RemoteActor.classLoader = getClass().getClassLoader()
    alive(9000)
    register('mandelbench, this)
    loop {
      react {
        case Render(b, compute) =>
          val pixelMap = new HashMap[Sample, Color]
          for (sample <- b) {
            pixelMap.put(sample, compute(sample))
          }
          reply(pixelMap)
      }
    }
  }
}

case class Start(clazz: Class[_ <: scala.actors.Actor])

case object Stop

class Server extends scala.actors.Actor with Serializable {
  import scala.actors.Actor
  import scala.actors.remote.RemoteActor.{alive, register}
  import scala.actors.remote.RemoteActor

  RemoteActor.classLoader = getClass().getClassLoader()
  var numStarted = 0
  def act() {
    alive(9000)
    register('server, this)
    println("remote start server running...")
    loop {
      react {
        case Start(clazz) =>
          val a: Actor = clazz.newInstance()
          a.start()
          numStarted += 1
          reply()

        case Stop =>
          println("remote start server started " + numStarted + " remote actors")
          exit()
      }
    }
  }
}

class EventActor extends scala.actors.Actor {
  import scala.collection.mutable.HashMap

  def act() {
    loop {
      react {
        case Render(b, compute) =>
          val pixelMap = new HashMap[Sample, Color]
          for (sample <- b) {
            pixelMap.put(sample, compute(sample))
          }
          reply(pixelMap)

        case Stop => exit()
      }
    }
  }

  override def exceptionHandler = {
    case e: Exception => println(e.getMessage())
  }
}

// Renderer that uses event-based actors with futures
object EventFuturesActorsRenderer extends Renderer {
  import scala.actors.Actor
  import scala.actors.Actor._
  import scala.collection.mutable.HashMap

  override def render(sampler: Sampler, compute: Sample => Color, pixelBuffer: PixelBuffer) {
    val actorCount = Config.rendererActorCount

    println("- Starting actors; number of actors: " + actorCount)

    // Create event-driven actors to do the computation for a batch of samples
    val computeActors = new Array[Actor](actorCount)
    for (i <- 0 until actorCount) computeActors(i) = new EventActor().start

    var i = 0
    val dataFutures = for (batch <- sampler.batches) yield {

      // Select the actor to send the next message to
      val computeActor = computeActors(i % actorCount); i += 1

      // Send the batch to the actor
      computeActor !! Render(batch,compute)
    }

    for (future <- dataFutures) {
      future.inputChannel.receive {
        case pixelMap: HashMap[Sample,Color] =>
          val pixelBufferActor = actor {
            react {
              case pixelMap: HashMap[Sample, Color] =>
                for (sample <- pixelMap.keys) {
                  val color:Option[Color] = pixelMap.get(sample)
                  pixelBuffer.add(sample, color.get)
                }
            }
          }

          // Send the pixelMap to the actor
          pixelBufferActor ! pixelMap

        case _ => println("- Error")
      }
    }

    for (i <- 0 until actorCount) computeActors(i) ! Stop

    // Wait for all the actors to finish
    println("- Waiting for actors to finish")
  }

  override def toString = "EventFuturesActorsRenderer"
}
