package experiments

import domain.{FeedbackTypes, Review}
import learning.GPMethod
import learning.core.PatternGroup
import org.apache.spark.SparkContext
import usecases.{NoisyLabelProviderFromComplexPatterns, GoldenLabelProvider}
import utilities.{DataSource}

class GPPatternGroupDirectClassification (
   targetFeedbackType: FeedbackTypes.Value,
   sc: SparkContext,
   db: DataSource
  ) extends Experiment {

  override var performance: Performance = _
  override val target: String = targetFeedbackType.toString
  override val name: String = "Predict with Learned Patterns"

  override def run = {
    val GPTrainingReviews: Seq[Review] = db.getTrainingReviews.filter(r => db.getAnnotationReviewIds.contains(r.id))
    val goldenLabelProvider = new GoldenLabelProvider(db.getReviewAnnotations)
    val goldenLabels: Seq[Int] = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => goldenLabelProvider.getLabelsForDefectReportClassification(GPTrainingReviews)
      case FeedbackTypes.IMPROVEMENT_REQUEST => goldenLabelProvider.getLabelsForImprovementRequestClassification(GPTrainingReviews)
    }

    val config = new GPConfiguration(500, 100, 0.2, 0.1, 0.05, 0.45, 0.5)
    val patternLearning = new GPMethod(config, GPTrainingReviews, goldenLabels, targetFeedbackType)
    val patternGroup: PatternGroup = patternLearning.runGPMethod
    val noisyLabelProvider = new NoisyLabelProviderFromComplexPatterns(patternGroup)
    val testReviews = db.getTestReviews

    val testLabels = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => goldenLabelProvider.getLabelsForDefectReportClassification(testReviews)
      case FeedbackTypes.IMPROVEMENT_REQUEST => goldenLabelProvider.getLabelsForImprovementRequestClassification(testReviews)
    }

    val predictedLabels = noisyLabelProvider.getSentenceLabels(testReviews)
    val scoreAndLabels: Seq[(Int, Int)] = predictedLabels zip testLabels
    performance = evaluator.getPerformance(scoreAndLabels)
  }

}
