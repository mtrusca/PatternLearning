package experiments

import domain.{TreePatterns, Review, FeedbackTypes}
import learning.GPMethod
import learning.core.PatternGroup
import usecases.{NoisyLabelProviderFromComplexPatterns, GoldenLabelProvider}
import utilities.DataSource

/**
  * Created by Gino on 23/02/16.
  */

class ExperimentOneBReviewLevel(
  targetFeedbackType: FeedbackTypes.Value,
  db: DataSource
) extends Experiment {
  override val name: String = "Patterns B"
  override var performance: Performance = _
  override val target: String = targetFeedbackType.toString

  override def run: Unit = {

//    val trainingReviews: Seq[Review] = db.getTrainingReviews.filter(r => db.getAnnotationReviewIds.contains(r.id))
    val goldenLabelProvider = new GoldenLabelProvider(db.getReviewAnnotations)
//    val goldenLabels: Seq[Int] = targetFeedbackType match {
//      case FeedbackTypes.DEFECT_REPORT => goldenLabelProvider.getDefectLabelsForReviews(trainingReviews)
//      case FeedbackTypes.IMPROVEMENT_REQUEST => goldenLabelProvider.getImprovementLabelsForReviews(trainingReviews)
//    }
//
//    val config = new GPConfiguration(1000, 2, 0.10, 0.15, 0.05, 0.45, 0.5)
//    val patternLearning = new GPMethod(config, trainingReviews, goldenLabels, targetFeedbackType, true)
//    val patternGroup: PatternGroup = patternLearning.runGPMethodTimed()

    val patternGroup: PatternGroup = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => TreePatterns.defectPatternGroup
      case FeedbackTypes.IMPROVEMENT_REQUEST => TreePatterns.improvementPatternGroup
    }

    val noisyLabelProvider = new NoisyLabelProviderFromComplexPatterns(patternGroup)
    val testReviews = db.getTestReviews

    val testLabels = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => goldenLabelProvider.getDefectLabelsForReviews(testReviews)
      case FeedbackTypes.IMPROVEMENT_REQUEST => goldenLabelProvider.getImprovementLabelsForReviews(testReviews)
    }

    val predictedLabels = noisyLabelProvider.getReviewLabels(testReviews)
    val pos = predictedLabels.filter(_ == 1)
    val neg = predictedLabels.filter(_ == 0)

    val scoreAndLabels: Seq[(Int, Int)] = predictedLabels zip testLabels
    performance = evaluator.getPerformance(scoreAndLabels)
    patternGroup.storePatternsToFile()

  }

}
