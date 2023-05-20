package cheatsheet

import cats.effect.std.Queue
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import fs2.{Chunk, INothing, Pipe, Pull, Pure, Stream}

object Model {
  case class Actor(id: Int, firstName: String, lastName: String)
}

object Data {
  import Model.Actor
  // Justice League
  val henryCavil: Actor = Actor(0, "Henry", "Cavill")
  val galGodot: Actor = Actor(1, "Gal", "Godot")
  val ezraMiller: Actor = Actor(2, "Ezra", "Miller")
  val benFisher: Actor = Actor(3, "Ben", "Fisher")
  val rayHardy: Actor = Actor(4, "Ray", "Hardy")
  val jasonMomoa: Actor = Actor(5, "Jason", "Momoa")

  // Avengers
  val scarlettJohansson: Actor = Actor(6, "Scarlett", "Johansson")
  val robertDowneyJr: Actor = Actor(7, "Robert", "Downey Jr.")
  val chrisEvans: Actor = Actor(8, "Chris", "Evans")
  val markRuffalo: Actor = Actor(9, "Mark", "Ruffalo")
  val chrisHemsworth: Actor = Actor(10, "Chris", "Hemsworth")
  val jeremyRenner: Actor = Actor(11, "Jeremy", "Renner")
  val tomHolland: Actor = Actor(13, "Tom", "Holland")
  val tobeyMaguire: Actor = Actor(14, "Tobey", "Maguire")
  val andrewGarfield: Actor = Actor(15, "Andrew", "Garfield")
}

object MyFS2 {
  import Model.Actor
  import Data._

  // Create a pure Stream
  val jlActors: Stream[Pure, Actor] = Stream(
    henryCavil,
    galGodot,
    ezraMiller,
    benFisher,
    rayHardy,
    jasonMomoa
  )

  // Create a pure Stream from an element or a Sequence
  val tomHollandStream: Stream[Pure, Actor] = Stream.emit(tomHolland)
  val spiderMen: Stream[Pure, Actor] = Stream.emits(
    List(
      tomHolland,
      tobeyMaguire,
      andrewGarfield
    ))

  // Convert pure Stream back to Sequences
  val jlActorList: List[Actor] = jlActors.toList
  val jlActorVector: Vector[Actor] = jlActors.toVector

  // Create an infinite stream by repeating
  val infiniteJlActors: Stream[Pure, Actor] = jlActors.repeat

  // Take a number of elements from a Stream
  val repeatedJLActorsList: List[Actor] = infiniteJlActors.take(12).toList

  // To map a Pure to an IO we use covary
  val liftedJlActors: Stream[IO, Actor] = jlActors.covary[IO]

  // Stream with effects
  val savingTomHolland: Stream[IO, Unit] = Stream
    .eval { // eval takes an IO effect and returns a Stream that will evalute the IO when pulled
      IO {
        println(s"Saving actor $tomHolland")
        Thread.sleep(1000)
        println("Finished")
      }
    }

  // How to pull IO from Stream? We cannot use .toList because it is not pure.
  // What we do is compile
  val compiledStream: IO[Unit] =
    savingTomHolland.compile.drain // drain discard any effect output

  val jlActorsEffectfulList: IO[List[Actor]] = liftedJlActors.compile.toList

  // CHUNKS: Every stream is made of chunks
  val avengersActors: Stream[Pure, Actor] = Stream.chunk(
    Chunk.array( // We can create a Chunk from an Option, Seq, Queue etc
      Array(
        scarlettJohansson,
        robertDowneyJr,
        chrisEvans,
        markRuffalo,
        chrisHemsworth,
        jeremyRenner
      )))

  // ++ two streams
  val dcAndMarvelSuperheroes: Stream[Pure, Actor] = jlActors ++ avengersActors

  // Stream is a monad on the 0 type parameter
  val printedJlActors: Stream[IO, Unit] = jlActors.flatMap { actor =>
    Stream.eval(IO.println(actor))
  }

  val evalMappedJlActors: Stream[IO, Unit] = jlActors.evalMap(IO.println)
}

object Fs2Tutorial extends IOApp {
  import MyFS2._
  override def run(args: List[String]): IO[ExitCode] = {
    savingTomHolland.compile.drain.as(ExitCode.Success)
  }
}
