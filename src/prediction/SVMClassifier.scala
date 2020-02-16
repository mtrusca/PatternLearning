package prediction

import domain.{Review, FeedbackTypes}
import org.apache.spark.SparkContext
import org.apache.spark.mllib.classification.{SVMModel, SVMWithSGD}
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.util.MLUtils
import org.apache.spark.rdd.RDD
import utilities.{DataPreparator}

class SVMClassifier(sc: SparkContext,
  targetFeedbackType: FeedbackTypes.Value,
  bowIndices: Seq[String],
  useNGrams: Boolean,
  reviewLevel: Boolean) extends Classifier {
  override val name: String = "SVM Classifier"
  val dataPreparator = new DataPreparator

  override def train(trainingDocs: Seq[Review], trainingLabels: Seq[Int]): SVMModel = {
    val trainingExamples = reviewLevel match {
      case true => dataPreparator.getSparseMatrixInLibSVMFormatReviewLevel(trainingDocs, bowIndices, useNGrams)
      case false => dataPreparator.getSparseMatrixInLibSVMFormatSentenceLevel(trainingDocs, bowIndices, useNGrams)
    }

    val fileName = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => "defect_training_libsvm.txt"
      case FeedbackTypes.IMPROVEMENT_REQUEST => "improvement_training_libsvm.txt"
    }

    if(trainingLabels.size != trainingExamples.size) {
      throw new IllegalStateException("Examples and Labels are out of sync")
    } else {
      dataPreparator.writeDataPointsToFile(trainingLabels, trainingExamples, fileName)
      val training: RDD[LabeledPoint] = MLUtils.loadLibSVMFile(sc, "data/mllib/" + fileName)
      val numIterations = 100 // 100
      val regParam = 0.0001 // typical ranges from 0,000001 to 10000
      var model = SVMWithSGD.train(training, numIterations, 1.0, regParam, 1.0)
      model.clearThreshold()
      model
    }

  }

  override def predict(testDocs: Seq[Review], testLabels: Seq[Int], model: SVMModel) = {
    val testExamples = reviewLevel match {
      case true => dataPreparator.getSparseMatrixInLibSVMFormatReviewLevel(testDocs, bowIndices, useNGrams)
      case false => dataPreparator.getSparseMatrixInLibSVMFormatSentenceLevel(testDocs, bowIndices, useNGrams)
    }

    val fileName = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => "defect_test_libsvm.txt"
      case FeedbackTypes.IMPROVEMENT_REQUEST => "improvement_test_libsvm.txt"
    }

    if(testExamples.size != testLabels.size) {
      throw new IllegalStateException("Examples and Labels are out of sync")
    } else {

      dataPreparator.writeDataPointsToFile(testLabels, testExamples, fileName)

      val test = MLUtils.loadLibSVMFile(sc, "data/mllib/" + fileName)

      // Compute raw scores on the test set.
      val scoreAndLabels = test.map { point =>
        val score = model.predict(point.features)
        (score, point.label)
      }
      scoreAndLabels
    }
  }

}
