package utilities


import breeze.text.analyze.PorterStemmer
import breeze.text.tokenize.PTBTokenizer
import domain.{stopWords, Review}


/**
  * Bag-of-Words Featurizer:
  *
  * This class transforms a collection of Reviews to create Bag-of-Words Vector representations of each.
  * Furthermore, has a method to transform a collection of Reviews to a One-Based Matrix representation
  * according to the LibSVM specified format: http://www.csie.ntu.edu.tw/~cjlin/libsvm/
  *
  */

class BowFeaturizer {

  val stemmer = new PorterStemmer

  def getBowNGramIndices(n: Int, reviews: Seq[Review]) = {
    reviews
      .flatMap(r => getUniqueNGramsInReview(n, r))
      .distinct
  }

  def getBowIndicesWithoutStopwords(reviews: Seq[Review]) = {
    getBowIndices(reviews).filterNot(w => stopWords.isStopword(w))
  }

  def getBowIndices(reviews: Seq[Review]): Seq[String] = {
    reviews.flatMap(getUniqueTokensInReview)
      .distinct
  }

  def getUniqueTokensInReview(review: Review): Seq[String] = {
    review.body
      .flatMap(getTokens)
      .distinct
  }

  def getUniqueNGramsInReview(n: Int, review: Review): Seq[String] = {
    review.body
      .flatMap(s => getNGrams(n, s))
      .distinct
  }

  def getNGrams(n: Int,sentence: String) = {
    val words = getTokens(sentence)
    words.sliding(n).map( p => p.mkString(" ")).toList
  }

  def getTokens(rawSen: String) = {
    PTBTokenizer(rawSen)
      .filterNot(stopWords.isStopword)
      .map(stemmer)
      .toList
  }

}
