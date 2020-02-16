import domain.FeedbackTypes
import experiments._
import org.apache.spark.{SparkConf, SparkContext}
import utilities.DataSource

import java.io._

object Main extends App {
  val conf = new SparkConf().setAppName("Feedback Classifier").setMaster("local")
  val sc = new SparkContext(conf)
  val db = new DataSource("evernote")

  val availableMethods = Map(
    1 -> Seq(
      new ExperimentZeroSVMFair(FeedbackTypes.DEFECT_REPORT, sc, db),
      new ExperimentZeroSVMFair(FeedbackTypes.IMPROVEMENT_REQUEST, sc, db)
    ),
    2 -> Seq(
      new ExperimentOneAReviewLevel(FeedbackTypes.DEFECT_REPORT, db),
      new ExperimentOneAReviewLevel(FeedbackTypes.IMPROVEMENT_REQUEST, db)
    ),
    3 -> Seq(
      new ExperimentOneBReviewLevel(FeedbackTypes.DEFECT_REPORT, db),
      new ExperimentOneBReviewLevel(FeedbackTypes.IMPROVEMENT_REQUEST, db)
    ),
    4 -> Seq(
      new ExperimentTwoReviewLevel(FeedbackTypes.DEFECT_REPORT, sc, db),
      new ExperimentTwoReviewLevel(FeedbackTypes.IMPROVEMENT_REQUEST, sc, db)
    ),
    5 -> Seq(
      new ExperimentThreeReviewLevel(FeedbackTypes.DEFECT_REPORT, sc, db),
      new ExperimentThreeReviewLevel(FeedbackTypes.IMPROVEMENT_REQUEST, sc, db)
    ),
    6 -> Seq(
      new ExperimentFourAReviewLevel(FeedbackTypes.DEFECT_REPORT, sc, db),
      new ExperimentFourAReviewLevel(FeedbackTypes.IMPROVEMENT_REQUEST, sc, db)
    ),
    7 -> Seq(
      new ExperimentFourBReviewLevel(FeedbackTypes.DEFECT_REPORT, sc, db),
      new ExperimentFourBReviewLevel(FeedbackTypes.IMPROVEMENT_REQUEST, sc, db)
    ),
    8 -> Seq(
      new ExperimentZeroSVMFair(FeedbackTypes.DEFECT_REPORT, sc, db),
      new ExperimentZeroSVMFair(FeedbackTypes.IMPROVEMENT_REQUEST, sc, db),
      new ExperimentOneAReviewLevel(FeedbackTypes.DEFECT_REPORT, db),
      new ExperimentOneAReviewLevel(FeedbackTypes.IMPROVEMENT_REQUEST, db),
      new ExperimentOneBReviewLevel(FeedbackTypes.DEFECT_REPORT, db),
      new ExperimentOneBReviewLevel(FeedbackTypes.IMPROVEMENT_REQUEST, db),
      new ExperimentTwoReviewLevel(FeedbackTypes.DEFECT_REPORT, sc, db),
      new ExperimentTwoReviewLevel(FeedbackTypes.IMPROVEMENT_REQUEST, sc, db),
      new ExperimentThreeReviewLevel(FeedbackTypes.DEFECT_REPORT, sc, db),
      new ExperimentThreeReviewLevel(FeedbackTypes.IMPROVEMENT_REQUEST, sc, db),
      new ExperimentFourAReviewLevel(FeedbackTypes.DEFECT_REPORT, sc, db),
      new ExperimentFourAReviewLevel(FeedbackTypes.IMPROVEMENT_REQUEST, sc, db),
      new ExperimentFourBReviewLevel(FeedbackTypes.DEFECT_REPORT, sc, db),
      new ExperimentFourBReviewLevel(FeedbackTypes.IMPROVEMENT_REQUEST, sc, db)
    )
  )

  val methodsToRun = determineMethodsToRun(args)
  runMethods(availableMethods, methodsToRun)


  def runMethods(availableMethods: Map[Int, Seq[Experiment]], methodsToRun: Seq[Int]) = {
    val inputIsValid = (methodsToRun.min > 0) && (methodsToRun.max < 9)
    if(inputIsValid) {
      val experiments = availableMethods.filter(m => methodsToRun.contains(m._1)).flatMap(_._2)
      experiments.foreach(_.run)
      val allReports = experiments.map(_.getReport).mkString
      reportExperiments(allReports)
    } else {
      throw new IllegalArgumentException("The argument(s) you provided are invalid, please check the readme.")
    }
  }


  def reportExperiments(report: String) = {
    val timestamp: Long = System.currentTimeMillis / 1000
    val file = new File("results/experiment_results_%s.txt".format(timestamp))
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(report)
    bw.close()
  }


  def determineMethodsToRun(arguments: Seq[String]): Seq[Int] = {
    if(arguments.isEmpty){
      Seq(8)
    } else {
      arguments.map(x => {
        val potentialNumber: Option[Int] = toInt(x)
        potentialNumber.getOrElse(0)
      })
    }
  }


  def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: Exception => None
    }
  }

}


