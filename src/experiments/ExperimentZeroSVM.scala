package experiments

import domain.{Review, FeedbackTypes}
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import prediction.SVMClassifier
import usecases.GoldenLabelProvider
import utilities.{BowFeaturizer, DataSource}
import scala.collection.mutable.ListBuffer

class ExperimentZeroSVM(
  targetFeedbackType: FeedbackTypes.Value,
  sc: SparkContext,
  db: DataSource
  )extends Experiment{

  override val name = "Standard SVM"
  override var performance: Performance = _
  override val target = targetFeedbackType.toString

  override def run = {

    val annotations = db.getReviewAnnotations
    val idsOfLabelledReviews = db.reviewsWithLabels

    val allReviews: Seq[Review] = db.getReviews.filter(r => idsOfLabelledReviews.contains(r.id))

    val numberOfFolds = 10
    val foldSize = allReviews.size / numberOfFolds
    val folds = allReviews.grouped(foldSize).toList

    var scores = new ListBuffer[Performance]()

    folds.zipWithIndex.foreach(foldWithIndex => {
      val fold = foldWithIndex._1
      val index = foldWithIndex._2

      val testDocs: Seq[Review] = folds(index)
      val trainingDocs: Seq[Review] = (folds.take(index) ++ folds.drop(index + 1)).flatMap(x => x)

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
      scores += evaluator.getPerformanceFromSpark(scoreAndLabels)

    })


    val averageF1 = average(scores.map(_.f1))
    val averagePrecision = average(scores.map(_.precision))
    val averageRecall = average(scores.map(_.recall))

    performance = new Performance(averageF1, averagePrecision, averageRecall)

  }

  def average[T]( ts: Iterable[T] )( implicit num: Numeric[T] ) = {
    num.toDouble( ts.sum ) / ts.size
  }

}
