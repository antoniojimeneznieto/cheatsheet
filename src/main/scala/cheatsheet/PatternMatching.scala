package cheatsheet

import cats.effect.IO
import java.io.IOException
import scala.util.Random

object PatternMatching {

  // Matching a value
  def matchAValue() = {
    val aNumber = 100
    val ordinal = aNumber match {
      case 1 => "first"
      case 2 => "second"
      case 3 => "third"
      case _ => aNumber + "th" // ignore the English grammar will you please
    }
  }

  // Matching a class
  def matchAClass() = {
    case class Person(name: String, age: Int, favoriteMovies: List[String])
    val bob = Person("Bob", 34, List("Inception", "The Departed"))

    val describeBob = bob match {
      case Person(n, a, movies) =>
        s"$n is $a years old and likes ${movies.mkString(",")}"
      case _ => "I don't know what you're talking about"
    }
  }

  // Matching a list
  def matchAList() = {
    // By pattern
    val countingList = List(1, 2, 3, 100)
    val mustHaveThree = countingList match {
      case List(_, _, 3, somethingElse) =>
        s"A-HA! I've got a list with 3 as third element, I found $somethingElse after"
    }

    // Haskell-like preprending
    val startsWithOne = countingList match {
      case 1 :: someOtherElements =>
        "This lists starts with one, the rest is $someOtherElements"
    }

    def processList(numbers: List[Int]): String = numbers match {
      case Nil => ""
      case h :: t => h + " " + processList(t)
    }

    // List vararg pattern
    val dontCareAboutTheRest = countingList match {
      case List(_, 2, _*) =>
        "I only care that this list has 2 as second element"
    }

    // Matching last element
    val mustEndWithMeaningOfLife = countingList match {
      case List(1, 2, _) :+ 100 => "found 100"
    }

    val mustEndWithMeaningOfLife2 = countingList match {
      case List(1, _*) :+ 100 =>
        "I really don't care what comes before 100"
    }

  }

  // Matching a type
  def matchAType() = {
    def gimmeAValue(): Any = { 5 }

    val gimmeTheType = gimmeAValue() match {
      case _: String => "I have a string"
      case _: Int => "I have a number"
      case _ => "I have something else"
    }
  }

  // Match a try exception
  def matchATry() = {
    try {
      IO.delay(println("Computing...")) >> IO.pure(100)
    } catch {
      case _: IOException => "IO failed!"
      case _: Exception => "We could have prevented that!"
      case _: RuntimeException => "Something else crashed!"
    }
  }

  // Name Binding
  def nameBinding() = {
    case class Person(name: String, age: Int, favoriteMovies: List[String])
    def requestMoreInfo(p: Person): String = { "Can you give one more film" }
    val bob = Person("Bob", 34, List("Inception", "The Departed"))

    // DONT DO THIS
    val bobsInfo = bob match {
      case Person(name, age, movies) =>
        s"$name's info: ${requestMoreInfo(Person(name, age, movies))}" // Do not create a new Person
    }

    // Do name binding instead :)
    val bobsInfoCorrect = bob match {
      case p @ Person(name, _, _) => s"$name's info: ${requestMoreInfo(p)}"
    }

    // You can also bind an argument of the Person
    val bobsInception = bob match {
      case Person(name, _, movies @ List("Inception", _*)) =>
        s"$name REALLY likes Inception, some other movies too: $movies"
    }
  }

  // Conditional guards
  def conditionalGuards() = {
    def gimmeANumber(): Int = Random.nextInt()

    val ordinal = gimmeANumber() match {
      case 1 => "first"
      case 2 => "second"
      case 3 => "third"
      case n if n % 10 == 1 => n + "st"
      case n if n % 10 == 2 => n + "nd"
      case n if n % 10 == 3 => n + "rd"
      case n => n + "th"
    }
  }

  // Alternative patterns
  def alternativePatterns() = {
    // Use | instead of reusing the same expression
    val myOptimalList = List(1, 2, 3, 4, 5) match {
      case List(1, _, _) | List(43, _*) => "I like this list"
      case _ => "I don't like it"
    }
  }

}
