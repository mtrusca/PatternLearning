package domain

import experiments.Performance
import org.apache.spark.mllib.evaluation.BinaryClassificationMetrics
import org.apache.spark.rdd.RDD

class Evaluator {

  def getAccuracy(predictions: Seq[Int], answers: Seq[Int]): Double = {
    val tp: Int = (predictions zip answers) count { case(predicted, actual) => (predicted == actual) && (actual == 1) }
    val fp: Int = (predictions zip answers) count { case(predicted, actual) => (predicted != actual) && (actual == 1) }
    val fn: Int = (predictions zip answers) count { case(predicted, actual) => (predicted != actual) && (actual == 0) }
    getF1Measure(tp, fp, fn)
  }

  def getF1Measure(truePositives: Int, falsePositives: Int, falseNegatives: Int): Double = {
    val isComputable = truePositives != 0
    if(isComputable) {
      val precision = (truePositives.toDouble / (truePositives + falsePositives).toDouble)
      val recall = (truePositives.toDouble / (truePositives + falseNegatives).toDouble)
      ( 2.0 * precision * recall ) / ( precision + recall )
    } else {
      0.0
    }
  }

  def getPerformance(predictionsAndLabels: Seq[(Int, Int)]) = {
    val predictions = predictionsAndLabels.map(_._1)
    val answers = predictionsAndLabels.map(_._2)
    val tp: Int = (predictions zip answers) count { case(predicted, actual) => (predicted == actual) && (actual == 1) }
    val fp: Int = (predictions zip answers) count { case(predicted, actual) => (predicted != actual) && (actual == 1) }
    val fn: Int = (predictions zip answers) count { case(predicted, actual) => (predicted != actual) && (actual == 0) }

    val isComputable = tp != 0
    if(isComputable) {
      val precision = (tp.toDouble / (tp + fp).toDouble)
      val recall = (tp.toDouble / (tp + fn).toDouble)
      val f1 = ( 2.0 * precision * recall ) / ( precision + recall )
      new Performance(f1, precision, recall)
    } else {
      throw new IllegalStateException("Not computable")
    }
  }

  def getPerformanceFromSpark(scoreAndLabels: RDD[(Double, Double)]) = {
    val metrics = new BinaryClassificationMetrics(scoreAndLabels)

    val beta = 1.0
    val f1Scores = metrics.fMeasureByThreshold(beta).collect // (threshold, F-Measure) pairs
    val bestThreshold = f1Scores.sortWith(_._2 > _._2).head._1

    val f1 = f1Scores.find(_._1 == bestThreshold).get._2
    val precision = metrics.precisionByThreshold().collect().find(_._1 == bestThreshold).get._2
    val recall = metrics.recallByThreshold().collect().find(_._1 == bestThreshold).get._2

    new Performance(f1, precision, recall)
  }

}
