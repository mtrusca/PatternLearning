package experiments

import domain.{TreePatterns, FeedbackTypes}
import learning.core.PatternGroup
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import prediction.SVMClassifier
import usecases.{NoisyLabelProviderFromComplexPatterns, GoldenLabelProvider}
import utilities.{BowFeaturizer, DataSource}

class ExperimentThreeReviewLevel(
  targetFeedbackType: FeedbackTypes.Value,
  sc: SparkContext,
  db: DataSource,
  shuffleData: Boolean = false
) extends Experiment {
  override val name: String = "Distant Supervision B"
  override var performance: Performance = _
  override val target: String = targetFeedbackType.toString

  override def run: Unit = {

    if(shuffleData) {
      db.shuffle
    }

    val trainingDocs = db.getTrainingReviews
    val testDocs = db.getTestReviews

    val patternGroup: PatternGroup = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => TreePatterns.defectPatternGroup
      case FeedbackTypes.IMPROVEMENT_REQUEST => TreePatterns.improvementPatternGroup
    }

    val noisyLabelProvider = new NoisyLabelProviderFromComplexPatterns(patternGroup)
    val trainingLabels: Seq[Int] = noisyLabelProvider.getReviewLabels(trainingDocs)

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
