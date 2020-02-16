package experiments

import domain.{Annotation, TreePatterns, FeedbackTypes, Review}
import learning.GPMethod
import learning.core.PatternGroup
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import prediction.SVMClassifier
import usecases.{LexicalPatternMatcher, NoisyLabelProviderFromBasicPatterns, NoisyLabelProviderFromComplexPatterns, GoldenLabelProvider}
import utilities.{DataSource, BowFeaturizer}


class ExperimentFourAReviewLevel(
  targetFeedbackType: FeedbackTypes.Value,
  sc: SparkContext,
  db: DataSource
) extends Experiment {

  override val name = "SVM Mixed Labels A"
  override var performance: Performance = _
  override val target = targetFeedbackType.toString

  override def run = {

    val idsOfLabelledReviews = db.reviewsWithLabels

    val trainingDocsAll = db.getTrainingReviews
    val trainingDocsGolden = trainingDocsAll.filter(r => idsOfLabelledReviews.contains(r.id))
    val trainingDocsNoisy = trainingDocsAll.filter(r => !idsOfLabelledReviews.contains(r.id))

    val trainingDocs = trainingDocsGolden ++ trainingDocsNoisy

    val patternGroup: PatternGroup = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => TreePatterns.defectPatternGroup
      case FeedbackTypes.IMPROVEMENT_REQUEST => TreePatterns.improvementPatternGroup
    }

    val annotations = db.getReviewAnnotations
    val goldenLabelProvider = new GoldenLabelProvider(annotations)
    val noisyLabelProvider = new  NoisyLabelProviderFromBasicPatterns

    val trainingLabelsGolden = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => goldenLabelProvider.getDefectLabelsForReviews(trainingDocsGolden)
      case FeedbackTypes.IMPROVEMENT_REQUEST => goldenLabelProvider.getImprovementLabelsForReviews(trainingDocsGolden)
    }

    val trainingLabelsNoisy: Seq[Int] = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => noisyLabelProvider.getDefectLabelsForReviews(trainingDocsNoisy)
      case FeedbackTypes.IMPROVEMENT_REQUEST => noisyLabelProvider.getImprovementLabelsForReviews(trainingDocsNoisy)
    }

    val trainingLabels = trainingLabelsGolden ++ trainingLabelsNoisy

    val featurizer = new BowFeaturizer
    val bowIndices = featurizer.getBowIndices(trainingDocs)
    val svm = new SVMClassifier(sc, targetFeedbackType, bowIndices, false, true)
    val model = svm.train(trainingDocs, trainingLabels)

    val testDocs = db.getTestReviews
    val testLabels = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => goldenLabelProvider.getDefectLabelsForReviews(testDocs)
      case FeedbackTypes.IMPROVEMENT_REQUEST => goldenLabelProvider.getImprovementLabelsForReviews(testDocs)
    }

    val scoreAndLabels: RDD[(Double, Double)] = svm.predict(testDocs, testLabels, model)
    performance = evaluator.getPerformanceFromSpark(scoreAndLabels)
  }

}
