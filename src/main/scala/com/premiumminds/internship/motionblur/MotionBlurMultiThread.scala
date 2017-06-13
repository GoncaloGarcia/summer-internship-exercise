package com.premiumminds.internship.motionblur

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import java.util.concurrent.Executors
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Created by acamilo on 18-04-2016.
 */
object MotionBlurMultiThread extends MotionBlurFactory {
  /**
   * Method to start processing the data, this one should work in parallel
   * @param data matrix of integers
   * @param numberOfWorkers number of threads that should work in parallel
   * @return matrix of integers
   */
  override def run(data: Seq[Seq[Int]], numberOfWorkers: Int) = {
    implicit val executionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(numberOfWorkers))

    /**
     * Divides two integers and rounds up the result
     * @return Rounded up result of integer division
     */
    def divide(dividendo: Double, divisor: Double): Int = {
      val resultado = dividendo / divisor
      math.ceil(resultado).toInt
    }

    /**
     * Iterates through the rows passed as parameter and for each tuple (i/row, j/column) applies the correct calculation
     * (Calculate the average value of the current position plus the one on top, on the left and beneath
     * @param i first row to be analyzed
     * @param j last row to be analyzed
     * @return a List of Lists which is the motion blurred matrix
     */
    def calculate(i: Int, k: Int): List[Int] = {
      val lengthX = data(0).length - 1;
      val lengthY = data.length - 1;
      val end = if (k > data.length - 1) data.length - 1 else k

      val lista = for (
        i <- i to end;
        j <- 0 to lengthX
      ) yield {
        (i, j) match {
          case (`lengthY`, 0) if (`lengthY` == 0) => divide(data(lengthY)(j), 1) //Edge case - If the matrix is a single row, the first element has no calculations
          case (`lengthY`, j) if (`lengthY` == 0) => divide(data(lengthY)(j) + data(lengthY)(j - 1), 2) // Edge case - If the matrix is a single row, all but the first only need to access their predecessor
          case (0, 0)                             => divide(data(i)(j) + data(i + 1)(j), 2) //Top left corner - Can only access the cell beneath
          case (0, j)                             => divide(data(i)(j) + data(i + 1)(j) + data(i)(j - 1), 3) //Top row - Can only access the cell beneath and to the left
          case (`lengthY`, 0)                     => divide(data(lengthY)(j) + data(lengthY - 1)(j), 2) //Bottom left corner - Can only access the cell on top
          case (`lengthY`, j)                     => divide(data(lengthY)(j) + data(lengthY - 1)(j) + data(lengthY)(j - 1), 3) //Bottom row - Can only access cells to the left and on top
          case (i, 0)                             => divide(data(i)(j) + data(i - 1)(j) + data(i + 1)(j), 3) //Left column - Can only access cells on top and bottom
          case (i, j)                             => divide(data(i)(j) + data(i - 1)(j) + data(i + 1)(j) + data(i)(j - 1), 4) //All others - Can access any cell required
        }
      }
      lista.toList
    }

    def startCalculations = {
      val stepSize = if (numberOfWorkers < data.length) divide(data.length, numberOfWorkers) else data.length
      val initialIndexes = List.range(0, data.length, stepSize)
      val futures = Future.traverse(initialIndexes)(i => Future { calculate(i, i + stepSize - 1) })
      //futures.map(e => e.flatten.grouped(data(0).length).toList.foreach(println))
      futures.map(e => e.flatten.grouped(data(0).length).toList)
    }

    if (data(0).isEmpty) Future { List(List()) } else startCalculations

  }
}
