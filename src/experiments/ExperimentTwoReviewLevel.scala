package experiments

import domain.FeedbackTypes
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import prediction.SVMClassifier
import usecases.{GoldenLabelProvider, NoisyLabelProviderFromBasicPatterns}
import utilities.{BowFeaturizer, DataSource}

class ExperimentTwoReviewLevel(
  targetFeedbackType: FeedbackTypes.Value,
  sc: SparkContext,
  db: DataSource,
  shuffleData: Boolean = false
) extends Experiment {
  override val name = "Distant Supervision A"
  override val target = targetFeedbackType.toString
  override var performance: Performance = _

  override def run = {

    if(shuffleData) {
      db.shuffle
    }

    val trainingDocs = db.getTrainingReviews
    val testDocs = db.getTestReviews

    val noisyLabelProvider = new NoisyLabelProviderFromBasicPatterns
    val trainingLabels: Seq[Int] = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => noisyLabelProvider.getDefectLabelsForReviews(trainingDocs)
      case FeedbackTypes.IMPROVEMENT_REQUEST => noisyLabelProvider.getImprovementLabelsForReviews(trainingDocs)
    }

    val featurizer = new BowFeaturizer
    val bowIndices = featurizer.getBowIndices(trainingDocs)
    val svm = new SVMClassifier(sc, targetFeedbackType, bowIndices, false, true)
    val model = svm.train(trainingDocs, trainingLabels)

    val annotations = db.getReviewAnnotations
    val goldenLabelProvider = new GoldenLabelProvider(annotations)

    val testLabels = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => goldenLabelProvider.getDefectLabelsForReviews(testDocs)
      case FeedbackTypes.IMPROVEMENT_REQUEST => goldenLabelProvider.getImprovementLabelsForReviews(testDocs)
    }

    val scoreAndLabels: RDD[(Double, Double)] = svm.predict(testDocs, testLabels, model)
    performance = evaluator.getPerformanceFromSpark(scoreAndLabels)

  }

}
