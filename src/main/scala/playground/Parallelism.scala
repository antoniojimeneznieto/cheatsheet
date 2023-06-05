package playground

import cats.effect._
import cats._
import cats.Traverse
import cats.syntax.traverse._
import cats.syntax.parallel._
import cheatsheet.util._

import scala.util.Random
import scala.concurrent.duration._
import cats.effect.kernel.Outcome.Succeeded
import cats.effect.kernel.Outcome.Errored
import cats.effect.kernel.Outcome.Canceled

import cats.syntax.functor._ // map
import cats.syntax.flatMap._ // flatMap

import cats.effect.IO.asyncForIO // implicit Async

object Parallelism {

  object gradesExample {
    def generateRandomGrades(id: Int, numberOfGrades: Int = 5): IO[(Int, List[Int])] =
      val students: List[Int] = (0 to numberOfGrades).toList
      for {
        _ <- IO(s"Fetching grades for student with id: $id from the database...").debug
        _ <- IO.sleep(1.second)
        res <- students.parTraverse(x => IO(Random.nextInt(10)))
      } yield (id, res)

    def generateRandomGradesPoly[F[_]](id: Int, numberOfGrades: Int = 5)(
        using F: Async[F],
        P: Parallel[F]): F[(Int, List[Int])] =
      val students: List[Int] = (0 to numberOfGrades).toList
      for {
        _ <- F.delay(s"Fetching grades for student with id: $id from the database...").debug
        _ <- F.sleep(1.second)
        res <- students.parTraverse(x => F.pure(Random.nextInt(10)))
      } yield (id, res)

    def fetchGrades(): IO[List[(Int, List[Int])]] = {
      val students = (0 to 100).toList
      for {
        _ <- IO("Fetching students grades from the database...").debug
        _ <- IO.sleep(200.millis)
        studentsGrades <- students.parTraverse(id => generateRandomGrades(id))
      } yield studentsGrades
    }

    def fetchGradesPoly[F[_]]()(
        using F: Async[F],
        P: Parallel[F]): F[List[(Int, List[Int])]] = {
      val students = (0 to 100).toList
      for {
        _ <- F.delay("Fetching students grades from the database...").debug
        _ <- F.sleep(200.millis)
        studentsGrades <- students.parTraverse(id => generateRandomGradesPoly(id))
      } yield studentsGrades

    }

    def computeAverage(id: Int, grades: List[Int]): IO[Int] = for {
      _ <- IO(s"Computing average of student with id: $id").debug
      _ <- IO.sleep(1.second)
      res <- IO(grades.sum / grades.length)
    } yield res

    def computeAveragePoly[F[_]](id: Int, grades: List[Int])(using F: Async[F]): F[Int] = for {
      _ <- F.delay(s"Computing average of student with id: $id").debug
      _ <- F.sleep(1.second)
      res <- F.pure(grades.sum / grades.length)
    } yield res

    def finalGrades(): IO[List[Int]] = for {
      _ <- IO("Starting computation...").debug
      studentsGrades <- fetchGrades()
      averages <- studentsGrades.parTraverse(computeAverage)
      _ <- IO("Computation finished").debug
      _ <- IO(s"The final list is: $averages").debug
    } yield averages

    def finalGradesPoly[F[_]]()(using F: Async[F], P: Parallel[F]): F[List[Int]] = for {
      _ <- F.delay("Starting computation...").debug
      studentsGrades <- fetchGradesPoly()
      averages <- studentsGrades.parTraverse(computeAveragePoly)
      _ <- F.delay("Computation finished").debug
      _ <- F.delay(s"The final list is: $averages").debug
    } yield averages

  }



}

object main extends IOApp.Simple {
  override def run: IO[Unit] = {
    Parallelism.gradesExample.finalGradesPoly().void
  }
}
