package com.premiumminds.internship.motionblur

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 *
 * Created by acamilo on 18-04-2016.
 */
@RunWith(classOf[JUnitRunner])
class MotionBlurTest extends FunSuite {
  /**
   * The corresponding implementations to test.
   *
   * If you want, you can make others :)
   *
   */
  val implementations: Map[String, MotionBlurFactory] =
    Map(
      "single thread" -> MotionBlurSingleThread,
      "multi thread" -> MotionBlurMultiThread)

  implementations.foreach {
    case (name, implementation) =>

      val t1 = System.nanoTime
      test(s"[$name] Motion blur with M1=\n${prettyPrint(MatrixData.M1)}\n") {
        val step1 = implementation.run(MatrixData.M1, 1)
        val result = Await.result(step1, 10.seconds)

        assert(result === List(
          List(3, 3, 4),
          List(4, 5, 6),
          List(6, 7, 8)))
      }
      println("TEST 1 - " + name + " - " + (System.nanoTime - t1) / 1e9d + " seconds")

      val t2 = System.nanoTime
      test(s"[$name] Motion blur with M2") {
        val step1 = implementation.run(MatrixData.M2, 5)
        val result = Await.result(step1, 10.seconds)
        //        result.foreach(println)

        assert(result === List(
          List(11, 8, 8, 8, 9, 9, 9, 10, 10, 10, 11, 11, 11, 12, 12, 12, 13, 13, 13, 14),
          List(8, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11),
          List(14, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11),
          List(8, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11),
          List(14, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11),
          List(9, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11),
          List(15, 12, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11),
          List(9, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11),
          List(14, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11),
          List(8, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11),
          List(14, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11),
          List(8, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11),
          List(14, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11),
          List(8, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11),
          List(14, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11),
          List(8, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11),
          List(14, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11),
          List(8, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11),
          List(14, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11),
          List(11, 14, 14, 13, 13, 13, 12, 12, 12, 11, 11, 11, 10, 10, 10, 9, 9, 9, 8, 8)))
      }
      println("TEST 2 - " + name + " - " + (System.nanoTime - t1) / 1e9d + " seconds")

      val t3 = System.nanoTime
      test(s"[$name] Motion blur with asymetric (wide) M3") {
        val step1 = implementation.run(MatrixData.M3, 5)
        val result = Await.result(step1, 10.seconds)

        assert(result === List(
          List(10, 23, 17, 13, 13),
          List(9, 17, 38, 6, 13),
          List(4, 18, 21, 13, 11),
          List(4, 4, 20, 4, 7)))
      }
      println("TEST 3 - " + name + " - " + (System.nanoTime - t1) / 1e9d + " seconds")

      val t4 = System.nanoTime
      test(s"[$name] Motion blur with asymetric (long) M4") {
        val step1 = implementation.run(MatrixData.M4, 5)
        val result = Await.result(step1, 10.seconds)

        assert(result === List(
          List(10, 23, 17, 13),
          List(9, 17, 38, 6),
          List(4, 18, 21, 13),
          List(6, 9, 20, 3),
          List(6, 12, 17, 7)))
      }
      println("TEST 4 - " + name + " - " + (System.nanoTime - t4) / 1e9d + " seconds")

      val t5 = System.nanoTime
      test(s"[$name] Motion blur with single row M5") {
        val step1 = implementation.run(MatrixData.M5, 5)
        val result = Await.result(step1, 10.seconds)

        assert(result === List(List(1, 118, 119, 4, 29, 30, 7, 8, 9, 57, 58, 67, 68, 14, 90, 91, 22, 23, 19, 20)))
      }
      println("TEST 5 - " + name + " - " + (System.nanoTime - t5) / 1e9d + " seconds")

      val t6 = System.nanoTime
      test(s"[$name] Motion blur with single column M6") {
        val step1 = implementation.run(MatrixData.M6, 5)
        val result = Await.result(step1, 10.seconds)

        assert(result === List(List(10),
          List (9),
          List (4),
          List (6),
          List (6)))
      }
      println("TEST 6 - " + name + " - " + (System.nanoTime - t6) / 1e9d + " seconds")
      
      val t7 = System.nanoTime
      test(s"[$name] Motion blur with empty matrix M7") {
        val step1 = implementation.run(MatrixData.M7, 5)
        val result = Await.result(step1, 10.seconds)

        assert(result === List(List()))
      }
      println("TEST 7 - " + name + " - " + (System.nanoTime - t7) / 1e9d + " seconds")
    /**
     * Implement more tests...
     */
  }

  def prettyPrint(data: Seq[Seq[Int]]) = data.map(_.mkString(" ")).mkString("\n")
}
