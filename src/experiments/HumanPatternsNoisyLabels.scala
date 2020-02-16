package experiments

import domain.{Review, FeedbackTypes}
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import prediction.SVMClassifier
import usecases.{GoldenLabelProvider, NoisyLabelProviderFromBasicPatterns, LexicalPatternMatcher}
import utilities.{BowFeaturizer, DataSource}

class HumanPatternsNoisyLabels(
    targetFeedbackType: FeedbackTypes.Value,
    sc: SparkContext,
    db: DataSource
  ) extends Experiment {

    override val name = "Distant Supervision with Manual Patterns"
    override var performance: Performance = _
    override val target = targetFeedbackType.toString

    override def run = {

      val testReviews = db.getTestReviews
      val trainingReviews: Seq[Review] = db.getTrainingReviews
      val featurizer = new BowFeaturizer

//      val bowIndices = featurizer.getBowIndicesWithoutStopwords(trainingReviews)
//      val svm = new SVMClassifier(sc, targetFeedbackType, bowIndices)

      val bonIndices = featurizer.getBowNGramIndices(2, trainingReviews)
      val svm = new SVMClassifier(sc, targetFeedbackType, bonIndices, true, true)

      val noisyLabelProvider = new NoisyLabelProviderFromBasicPatterns

      val trainingLabels: Seq[Int] = targetFeedbackType match {
        case FeedbackTypes.DEFECT_REPORT => noisyLabelProvider.getDefectLabelsForSentences(trainingReviews)
        case FeedbackTypes.IMPROVEMENT_REQUEST => noisyLabelProvider.getImprovementLabelsForSentences(trainingReviews)
      }

      val model = svm.train(trainingReviews, trainingLabels)


      val annotations = db.getReviewAnnotations
      val goldenLabelProvider = new GoldenLabelProvider(annotations)

      val testLabels = targetFeedbackType match {
        case FeedbackTypes.DEFECT_REPORT => goldenLabelProvider.getLabelsForDefectReportClassification(testReviews)
        case FeedbackTypes.IMPROVEMENT_REQUEST => goldenLabelProvider.getLabelsForImprovementRequestClassification(testReviews)
      }

      val scoreAndLabels: RDD[(Double, Double)] = svm.predict(testReviews, testLabels, model)
      performance = evaluator.getPerformanceFromSpark(scoreAndLabels)

    }


}
