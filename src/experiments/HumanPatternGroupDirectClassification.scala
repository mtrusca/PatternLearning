package experiments

import domain.{FeedbackTypes}
import org.apache.spark.SparkContext
import usecases.{LexicalPatternMatcher, NoisyLabelProviderFromBasicPatterns, GoldenLabelProvider}
import utilities.{DataSource}

class HumanPatternGroupDirectClassification (
  targetFeedbackType: FeedbackTypes.Value,
  sc: SparkContext,
  db: DataSource
) extends Experiment {

  override var performance: Performance = _
  override val target: String = targetFeedbackType.toString
  override val name: String = "Predict with Human Crafted Patterns"

  override def run = {

    val testReviews = db.getTestReviews

    val goldenLabelProvider = new GoldenLabelProvider(db.getReviewAnnotations)
    val testLabels = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => goldenLabelProvider.getLabelsForDefectReportClassification(testReviews)
      case FeedbackTypes.IMPROVEMENT_REQUEST => goldenLabelProvider.getLabelsForImprovementRequestClassification(testReviews)
    }

    val noisyLabelProvider = new NoisyLabelProviderFromBasicPatterns()
    val predictedLabels = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => noisyLabelProvider.getDefectLabelsForSentences(testReviews)
      case FeedbackTypes.IMPROVEMENT_REQUEST => noisyLabelProvider.getImprovementLabelsForSentences(testReviews)
    }
    val scoreAndLabels: Seq[(Int, Int)] = predictedLabels zip testLabels
    performance = evaluator.getPerformance(scoreAndLabels)
  }

}
