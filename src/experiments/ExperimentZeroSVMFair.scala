package experiments

import domain.{Review, TreePatterns, FeedbackTypes}
import learning.core.PatternGroup
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import prediction.SVMClassifier
import usecases.{GoldenLabelProvider, NoisyLabelProviderFromComplexPatterns}
import utilities.{BowFeaturizer, DataSource}

class ExperimentZeroSVMFair(
  targetFeedbackType: FeedbackTypes.Value,
  sc: SparkContext,
  db: DataSource
) extends Experiment {
  override val name: String = "Standard SVM (Fair)"
  override var performance: Performance = _
  override val target: String = targetFeedbackType.toString

  override def run: Unit = {

    val annotations = db.getReviewAnnotations
    val testDocs = db.getTestReviews

    val idsOfLabelledReviews = db.reviewsWithLabels
    val idsOfTestReviews = testDocs.map(_.id)

    val trainingDocs: Seq[Review] = db.getReviews.filter(r => (idsOfLabelledReviews.contains(r.id) || idsOfTestReviews.contains(r.id)))

    val goldenLabelProvider = new GoldenLabelProvider(annotations)
    val trainingLabels = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => goldenLabelProvider.getDefectLabelsForReviews(trainingDocs)
      case FeedbackTypes.IMPROVEMENT_REQUEST => goldenLabelProvider.getImprovementLabelsForReviews(trainingDocs)
    }

    val featurizer = new BowFeaturizer
    val bowIndices = featurizer.getBowIndices(trainingDocs)
    val svm = new SVMClassifier(sc, targetFeedbackType, bowIndices, false, true)
    val model = svm.train(trainingDocs, trainingLabels)

    val testLabels = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => goldenLabelProvider.getDefectLabelsForReviews(testDocs)
      case FeedbackTypes.IMPROVEMENT_REQUEST => goldenLabelProvider.getImprovementLabelsForReviews(testDocs)
    }

    val scoreAndLabels: RDD[(Double, Double)] = svm.predict(testDocs, testLabels, model)
    performance = evaluator.getPerformanceFromSpark(scoreAndLabels)
  }
}


