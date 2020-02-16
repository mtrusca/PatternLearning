package usecases

import domain.{Parser, Review, LabelProvider}
import learning.core.{Token, PatternGroup}


class NoisyLabelProviderFromComplexPatterns(patternGroup: PatternGroup) extends LabelProvider {

  def getReviewLabels(reviews: Seq[Review]): Seq[Int] = {
    reviews.map(classifyReviewForGivenPatternGroup).map(boolToInt)
  }

  def getSentenceLabels(reviews: Seq[Review]): Seq[Int] = {
    getLabelsForTask(reviews, classifySentenceForGivenPatternGroup)
  }

  def classifyReviewForGivenPatternGroup(review: Review): Boolean = {
    val fullReview = review.body.mkString(" ")
    if(fullReview == "") {
      false
    } else {
      classifySentenceForGivenPatternGroup(fullReview)
    }
  }

  def classifySentenceForGivenPatternGroup(sentence: String): Boolean = {
    val parsedSentence = Parser.parseSentence(sentence)
    val tokens = (parsedSentence.words zip parsedSentence.tags.get) map { case(word, tag) => new Token(word, tag) }
    patternGroup.patterns.exists(_.matches(tokens))
  }

}
