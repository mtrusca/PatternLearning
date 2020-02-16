package experiments

import domain.FeedbackTypes
import org.apache.spark.SparkContext
import usecases.{NoisyLabelProviderFromBasicPatterns, LexicalPatternMatcher, GoldenLabelProvider}
import utilities.DataSource

class ExperimentOneAReviewLevel(
  targetFeedbackType: FeedbackTypes.Value,
  db: DataSource
) extends Experiment {
  override val name: String = "Patterns A"
  override var performance: Performance = _
  override val target: String = targetFeedbackType.toString

  override def run: Unit = {
    val testReviews = db.getTestReviews

    val noisyLabelProvider = new NoisyLabelProviderFromBasicPatterns
    val predictedLabels = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => noisyLabelProvider.getDefectLabelsForReviews(testReviews)
      case FeedbackTypes.IMPROVEMENT_REQUEST => noisyLabelProvider.getImprovementLabelsForReviews(testReviews)
    }

    val goldenLabelProvider = new GoldenLabelProvider(db.getReviewAnnotations)
    val testLabels = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => goldenLabelProvider.getDefectLabelsForReviews(testReviews)
      case FeedbackTypes.IMPROVEMENT_REQUEST => goldenLabelProvider.getImprovementLabelsForReviews(testReviews)
    }

    val scoreAndLabels: Seq[(Int, Int)] = predictedLabels zip testLabels
    performance = evaluator.getPerformance(scoreAndLabels)
  }

}
