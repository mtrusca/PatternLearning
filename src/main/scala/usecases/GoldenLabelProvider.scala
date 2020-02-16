package usecases

import domain.{Review, Annotation, LabelProvider}

class GoldenLabelProvider(annotations: Seq[Annotation]) extends LabelProvider {

  def getDefectLabelsForReviews(reviews: Seq[Review]): Seq[Int] = {
    reviews.map(classifyReviewForDefectReport).map(boolToInt)
  }

  def getImprovementLabelsForReviews(reviews: Seq[Review]): Seq[Int] = {
    reviews.map(classifyReviewForImprovementRequest).map(boolToInt)
  }

  def getLabelsForDefectReportClassification(reviews: Seq[Review]): Seq[Int] = {
    getLabelsWithGivenFilter(reviews, classifySentenceForDefectReport)
  }

  def getLabelsForImprovementRequestClassification(reviews: Seq[Review]): Seq[Int] = {
    getLabelsWithGivenFilter(reviews, classifySentenceForImprovementRequest)
  }

  def getLabelsWithGivenFilter(reviews: Seq[Review], filter: (Review, String) => Boolean): Seq[Int] = {
    val allLabels = reviews.flatMap(r => r.body.map(filter(r, _)))
    allLabels.map(boolToInt)
  }

  def classifyReviewForDefectReport(review: Review): Boolean = {
    review.body.exists(classifySentenceForDefectReport(review, _))
  }

  def classifyReviewForImprovementRequest(review: Review): Boolean = {
    review.body.exists(classifySentenceForImprovementRequest(review, _))
  }

  def classifySentenceForDefectReport(review: Review, sentence: String): Boolean = {
    val potentialAnnotation = annotations.find(_.reviewId == review.id)
    potentialAnnotation match {
      case Some(annotation) => annotation.defects.contains(review.body.indexOf(sentence))
      case None => false
    }
  }

  def classifySentenceForImprovementRequest(review: Review, sentence: String): Boolean = {
    val potentialAnnotation = annotations.find(_.reviewId == review.id)
    potentialAnnotation match {
      case Some(annotation) => annotation.improvements.contains(review.body.indexOf(sentence))
      case None => false
    }
  }
}
