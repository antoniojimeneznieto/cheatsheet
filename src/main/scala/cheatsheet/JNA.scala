package cheatsheet

import com.sun.jna.Library
import com.sun.jna.Native
import com.sun.jna.Platform
import com.sun.jna.Structure
import com.sun.jna.NativeLong
import com.sun.jna.Pointer

import scala.main
import scala.beans.BeanProperty
import cats.effect.IO

object JNA {

  // Using a CLibrary:

  trait CLibrary extends Library {
    def printf(format: String, args: Object*): Int
  }

  trait CMath extends Library {
    def cosh(value: Double): Double
  }

  /* Types mapping:  */
  // char => byte
  // short => short
  // wchar_t => char
  // int => int
  // long => com.sun.jna.NativeLong
  // long long => long
  // float => float
  // double => double
  // char * => String

  // Using C Struct:

  // Assume we have a struct in C:
//   struct Point {
//     int x;
//     int y;
//   };
  class Tm extends Structure {
    @BeanProperty var tm_sec: Int = 0
    @BeanProperty var tm_min: Int = 0
    @BeanProperty var tm_hour: Int = 0
    @BeanProperty var tm_mday: Int = 0
    @BeanProperty var tm_mon: Int = 0
    @BeanProperty var tm_year: Int = 0
    @BeanProperty var tm_wday: Int = 0
    @BeanProperty var tm_yday: Int = 0
    @BeanProperty var tm_isdst: Int = 0

    override def getFieldOrder: java.util.List[String] =
      java
        .util
        .Arrays
        .asList(
          "tm_sec",
          "tm_min",
          "tm_hour",
          "tm_mday",
          "tm_mon",
          "tm_year",
          "tm_wday",
          "tm_yday",
          "tm_isdst")
  }

  // Using pointers:

  trait Stdc extends Library {
    def malloc(size: NativeLong): Pointer
    def free(p: Pointer): Unit // side effect therefore use delay
  }
}

object Main {
  import JNA._
  def main(args: Array[String]): Unit = {
    val cmath: CMath = Native.load("c", classOf[CMath])
    val res: Double = cmath.cosh(10)
    println(s"The value is $res")

    // TODO: There is a problem using the struct
    // val timeStruct = new Tm
    // timeStruct.tm_hour = 12
    // timeStruct.tm_min = 34
    // timeStruct.tm_sec = 56
    // println(s"Time: ${timeStruct.tm_hour}:${timeStruct.tm_min}:${timeStruct.tm_sec}")

    val stdc: Stdc = Native.load("c", classOf[Stdc])
    val myptr = stdc.malloc(NativeLong(10))
    IO.delay(stdc.free(myptr))
  }
}
