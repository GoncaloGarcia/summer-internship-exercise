package com.premiumminds.internship.motionblur

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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
    /**
     * Divides two integers and rounds up the result
     * @return Rounded up result of integer division
     */
   def divide(dividendo: Double, divisor: Double) : Int = {
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
    def calculate(i: Int, j: Int) : List[Int] = {
      val length = data(0).length - 1;
      val lista  = for (
        i <- i to j;
        j <- 0 to length
      ) yield {
        (i, j) match {
          case (0, 0) => divide(data(i)(j) + data(i + 1)(j), 2) //Top left corner - Can only access the cell beneath
          case (0, j) => divide(data(i)(j) + data(i + 1)(j)  +  data(i)(j - 1), 3) //Top row - Can only access the cell beneath and to the left
          case (`length`,0) => divide(data(length)(j)  + data(length - 1)(j),2) //Bottom left corner - Can only access the cell on top
          case (`length`,j) => divide(data(length)(j)  + data(length - 1)(j) + data(length)(j - 1),3) //Bottom row - Can only access cells to the left and on top
          case (i, 0) => divide(data(i)(j) + data(i-1)(j)+ data(i + 1)(j),3) //Left column - Can only access cells on top and bottom
          case (i, j) => divide(data(i)(j)  + data(i-1)(j)  + data(i + 1)(j)  +  data(i)(j - 1),4) //All others - Can access any cell required
        }
      }
      lista.toList
    }
    
   /*
    * Creates Futures to compute the calculations asynchronously
    * TODO: Create futures based on the number of workers passed as argument
    */
   val f1: Future[List[Int]] = Future {calculate(0,6)}
   val f2: Future[List[Int]] = Future {calculate(7,13)}
   val f3: Future[List[Int]] = Future {calculate(14,19)}
   
   
   /*
    * Starts all Futures simultaneously and returns the combination of each Future's calculations
    * as a matrix as soon as they're all finished
    */
   for{
       f <- f1;
       g <- f2;
       h <- f3
   } yield{
     println()
     List(f,g,h).flatten.grouped(data.length).foreach(println)
     List(f,g,h)
   }
  }    
}
