package com.premiumminds.internship.motionblur

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.concurrent.Await
import scala.concurrent.duration._

/**
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

  implementations.foreach { case (name, implementation) =>

    test(s"[$name] Motion blur with M1=\n${prettyPrint(MatrixData.M1)}\n") {
      val step1 = implementation.run(MatrixData.M1, 5)
      val result = Await.result(step1, 10.seconds)

      assert(result === List(
        List(3, 3, 4),
        List(4, 5, 6),
        List(6, 7, 8)
      ))
    }

    /**
      * Implement more tests...
      */
  }

  def prettyPrint(data: Seq[Seq[Int]]) = data.map(_.mkString(" ")).mkString("\n")
}
