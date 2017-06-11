package com.premiumminds.internship.motionblur

import scala.concurrent.Future

/**
  * Created by acamilo on 18-04-2016.
  */
trait MotionBlurFactory {
  /**
    * Method to start processing the data
    * @param data matrix of integers
    * @param numberOfWorkers number of threads that should work in parallel
    * @return matrix of integers
    */
  def run(data: Seq[Seq[Int]], numberOfWorkers: Int): Future[Seq[Seq[Int]]]
}
