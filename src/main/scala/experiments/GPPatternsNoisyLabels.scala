package experiments

import domain.{Review, FeedbackTypes}
import learning.GPMethod
import learning.core.PatternGroup
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import prediction.SVMClassifier
import usecases.{NoisyLabelProviderFromComplexPatterns, GoldenLabelProvider}
import utilities.{BowFeaturizer, DataSource}

class GPPatternsNoisyLabels(
    targetFeedbackType: FeedbackTypes.Value,
    sc: SparkContext,
    db: DataSource
  ) extends Experiment {

  override val name = "Distant Supervision with Learned Patterns"
  override var performance: Performance = _
  override val target = targetFeedbackType.toString

  override def run = {

    // GP Training Reviews, are training Reviews for which there are annotations (golden labels)
    val GPTrainingReviews: Seq[Review] = db.getTrainingReviews.filter(r => db.getAnnotationReviewIds.contains(r.id))

    val goldenLabelProvider = new GoldenLabelProvider(db.getReviewAnnotations)
    val goldenLabels: Seq[Int] = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => goldenLabelProvider.getLabelsForDefectReportClassification(GPTrainingReviews)
      case FeedbackTypes.IMPROVEMENT_REQUEST => goldenLabelProvider.getLabelsForImprovementRequestClassification(GPTrainingReviews)
    }

    val config = new GPConfiguration(500, 100, 0.2, 0.1, 0.05, 0.45, 0.5)
    val patternLearning = new GPMethod(config, GPTrainingReviews, goldenLabels, targetFeedbackType)
    val patternGroup: PatternGroup = patternLearning.runGPMethodTimed()

    println("Group accuracy: " + patternGroup.accuracy)
    patternGroup.patterns.foreach(_.print)


    val noisyLabelProvider = new NoisyLabelProviderFromComplexPatterns(patternGroup)

    val trainingReviews = db.getTrainingReviews
    val trainingLabels: Seq[Int] = noisyLabelProvider.getSentenceLabels(trainingReviews)

    val featurizer = new BowFeaturizer
    val bowIndices = featurizer.getBowNGramIndices(2, trainingReviews)
    val svm = new SVMClassifier(sc, targetFeedbackType, bowIndices, true, true)
    val model = svm.train(trainingReviews, trainingLabels)

    val testReviews = db.getTestReviews

    val testLabels = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => goldenLabelProvider.getLabelsForDefectReportClassification(testReviews)
      case FeedbackTypes.IMPROVEMENT_REQUEST => goldenLabelProvider.getLabelsForImprovementRequestClassification(testReviews)
    }

    val scoreAndLabels: RDD[(Double, Double)] = svm.predict(testReviews, testLabels, model)

    performance = evaluator.getPerformanceFromSpark(scoreAndLabels)

  }


}
