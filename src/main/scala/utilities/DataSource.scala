package utilities

import domain.{Annotation, Review}
import org.json4s.DefaultFormats
import org.json4s.native.JsonMethods._

import scala.io
import scala.util.Random

class DataSource(appName: String, basePath: String = System.getProperty("user.dir")){
  implicit val formats = DefaultFormats
  val cleaner = new DataCleaner
  val randomizer = new Random
  val testSampleRatio = 0.20

  val reviewIds: Seq[String] = getReviews.map(_.id)
  val reviewsWithLabels: Seq[String] = getLabelledReviewIds

  val testSize = (reviewIds.size.toDouble * testSampleRatio).round.toInt

  var testReviewIds: Seq[String] = getTestReviewIds
  var trainingReviewIds: Seq[String] = getTrainingReviewIds



  def getReviews: Seq[Review] = {
    val path = "%s/data/%s/reviews.json".format(basePath, appName)
    val inputJSON = parse(io.Source.fromFile(path).mkString)
    val reviewsJSON = inputJSON \ "reviews"
    reviewsJSON
      .extract[List[Review]]
      .map(cleaner.removeSentencesThatAreEmptyAfterCleanupFromReview)
  }

  def getTrainingReviews: Seq[Review] = {
    getReviews.filter(r => trainingReviewIds.contains(r.id))
  }

  def getTestReviews: Seq[Review] = {
    getReviews.filter(r => testReviewIds.contains(r.id))
  }

  def getLabelledReviewIds = {
    val targetReviewIds = getAnnotationReviewIds
    reviewIds.filter(targetReviewIds.contains)
  }

  def getLabelledReviews = {
    val targetIds = getLabelledReviewIds
    getReviews.filter(r => targetIds.contains(r.id))
  }

  def getTestReviewIds = {
    reviewsWithLabels.takeRight(testSize)
  }

  def getTrainingReviewIds = {
    reviewIds.filterNot(testReviewIds.contains)
  }

  def getAnnotationReviewIds: Seq[String] = {
    val path = "%s/data/%s/labels.json".format(basePath, appName)
    val inputJSON = parse(io.Source.fromFile(path).mkString)
    val testReviewIds = (inputJSON \ "history").extract[List[String]]
    testReviewIds
  }

  def getReviewAnnotations: Seq[Annotation] = {
    val path = "%s/data/%s/labels.json".format(basePath, appName)
    val inputJSON = parse(io.Source.fromFile(path).mkString)
    val reviewsLabelsJSON = inputJSON \ "annotations"
    reviewsLabelsJSON.extract[Seq[Annotation]]
  }

  def getReviewById(targetId: String)= getReviews.filter(_.id == targetId).head

  def shuffle = {
    val randomizedReviewIds = randomizer.shuffle(reviewsWithLabels)
    testReviewIds = randomizedReviewIds.take(testSize)
    trainingReviewIds = getTrainingReviewIds
  }
}
