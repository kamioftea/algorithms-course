import scala.annotation.tailrec

/**
 *
 * Created by Jeff on 03/04/2015.
 */
class Scheduling (val jobs: Vector[Job]) {
  lazy val weightedSumByDiff: BigInt = {
    val orderedJobs = jobs.sortWith(
      (job1, job2) =>
        if(job1.weight - job1.length == job2.weight - job2.length)
          job1.weight < job2.weight
        else
          job1.weight - job1.length < job2.weight - job2.length
    ).reverse

    weightedSum(orderedJobs.toList)
  }

  lazy val weightedSumByRatio: BigInt = {
    val orderedJobs = jobs.sortWith(
      (job1, job2) =>
        job1.weight.toDouble / job1.length < job2.weight.toDouble / job2.length
    ).reverse

    weightedSum(orderedJobs.toList)
  }

  def weightedSum(orderedJobs: List[Job]): BigInt =
  {
    @tailrec
    def doWeightedSum(remainingJobs: List[Job], elapsed: BigInt, acc: BigInt): BigInt = remainingJobs match {
      case Nil => acc
      case (job :: xs) =>
        val elapsedPost = elapsed + job.length
        doWeightedSum(xs, elapsedPost, acc + (elapsedPost * job.weight))
    }

    doWeightedSum(orderedJobs, 0, 0)
  }
}

object Scheduling {
  def fromFile(file:HeaderFile): Scheduling = {
    val jobs = file.lines.toVector.map((line) => {
      val split = line.span(_ != ' ')
      Job(split._1.toInt, split._2.trim.toInt)
    })

    new Scheduling(jobs)
  }
}

case class Job(weight: Int, length: Int)
