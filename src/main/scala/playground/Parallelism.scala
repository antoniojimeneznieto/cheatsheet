package playground

import cats.effect._
import cats.Traverse
import cats.syntax.traverse._
import cats.syntax.parallel._
import cheatsheet.util._

import scala.util.Random
import scala.concurrent.duration._

object Parallelism {

  object gradesExample {
    def generateRandomGrades(id: Int, numberOfGrades: Int = 5): IO[List[Int]] =
      val students: List[Int] = (0 to numberOfGrades).toList
      for {
        _ <- IO(s"Fetching grades for student with id: $id from the database...").debug
        _ <- IO.sleep(1.second)
        res <- students.parTraverse(x => IO(Random.nextInt(10)))
      } yield res

    def fetchGrades(): IO[List[List[Int]]] = {
      val students = (0 to 10).toList
      for {
        _ <- IO("Fetching students grades from the database...").debug
        _ <- IO.sleep(200.millis)
        studentsGrades <- students.parTraverse(id => generateRandomGrades(id))
      } yield studentsGrades
    }

    def computeAverage(grades: List[Int]): IO[Int] = for {
      _ <- IO("Computing average").debug
      _ <- IO.sleep(1.second)
      res <- IO(grades.sum / grades.length)
    } yield res

    def finalGrades: IO[List[Int]] = for {
      _ <- IO("Starting computation...").debug
      studentsGrades <- fetchGrades()
      averages <- studentsGrades.parTraverse(computeAverage)
      _ <- IO("Computation finished").debug
      _ <- IO(s"The final list is: $averages").debug
    } yield averages

  }

}

object main extends IOApp.Simple {
  override def run: IO[Unit] = {
    Parallelism.gradesExample.finalGrades.void
  }
}
