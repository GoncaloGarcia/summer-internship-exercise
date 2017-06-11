package com.premiumminds.internship.motionblur

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Created by acamilo on 18-04-2016.
 */
object MotionBlurSingleThread extends MotionBlurFactory {
  /**
   * Method to start processing the data, this one uses only the current thread
   * @param data matrix of integers
   * @param numberOfWorkers this parameter should be ignored
   * @return matrix of integers
   */
  def run(data: Seq[Seq[Int]], numberOfWorkers: Int) = {

    /**
     * Divides two integers and rounds up the result
     * @return Rounded up result of integer division
     */
    def divide(dividendo: Double, divisor: Double) : Int = {
      val resultado = dividendo / divisor
      math.ceil(resultado).toInt
    }

    /*
     * Iterates through the matrix and for each tuple (i/row, j/column) applies the correct calculation
     * (Calculate the average value of the current position plus the one on top, on the left and beneath
     * Returns a List of Lists which is the motion blurred matrix
     */
    Future {
       val length = data(0).length - 1;
      val lista  = for (
        i <- 0 to length;
        j <- 0 to length
      ) yield {
        (i, j) match {
          case (0, 0) => divide(data(i)(j) + data(i + 1)(j), 2)
          case (0, j) => divide(data(i)(j) + data(i + 1)(j)  +  data(i)(j - 1), 3)
          case (`length`,0) => divide(data(length)(j)  + data(length - 1)(j),2)
          case (`length`,j) => divide(data(length)(j)  + data(length - 1)(j) + data(length)(j - 1),3)
          case (i, 0) => divide(data(i)(j) + data(i-1)(j)+ data(i + 1)(j),3)
          case (i, j) => divide(data(i)(j)  + data(i-1)(j)  + data(i + 1)(j)  +  data(i)(j - 1),4)
        }
      }
      lista.toList.grouped(length + 1).toList.foreach(println)
      lista.grouped(length + 1).toList // Groups the list of values into a NxN matrix where N is the size of each column
    }
  }
}

