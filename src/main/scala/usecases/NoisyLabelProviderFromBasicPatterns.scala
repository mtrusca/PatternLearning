package usecases

import domain.{BasicPatterns, LabelProvider, Review}

class NoisyLabelProviderFromBasicPatterns extends LabelProvider {

  val matcher: LexicalPatternMatcher = new LexicalPatternMatcher

  def getDefectLabelsForReviews(reviews: Seq[Review]): Seq[Int] = {
    reviews.map(classifyReviewForDefectReport).map(boolToInt)
  }

  def getImprovementLabelsForReviews(reviews: Seq[Review]): Seq[Int] = {
    reviews.map(classifyReviewForImprovementRequest).map(boolToInt)
  }

  def getDefectLabelsForSentences(reviews: Seq[Review]): Seq[Int] = {
    getLabelsForTask(reviews, classifySentenceForDefectReport)
  }

  def getImprovementLabelsForSentences(reviews: Seq[Review]): Seq[Int] = {
    getLabelsForTask(reviews, classifySentenceForImprovementRequest)
  }

  def classifyReviewForImprovementRequest(review: Review): Boolean = {
    review.body.exists(classifySentenceForImprovementRequest)
  }

  def classifyReviewForDefectReport(review: Review): Boolean = {
    review.body.exists(classifySentenceForDefectReport)
  }

  def classifySentenceForDefectReport(sentence: String): Boolean = {
    BasicPatterns.defectPatterns
      .exists(matcher.patternMatchesSentence(_, sentence))
  }

  def classifySentenceForImprovementRequest(sentence: String): Boolean = {
    BasicPatterns.improvementPatterns
      .exists(matcher.patternMatchesSentence(_, sentence))
  }

}