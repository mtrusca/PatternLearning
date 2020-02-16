package utilities

import domain.{Review, stopWords}

class DataCleaner {

  def cleanSentence(sentence: String, ignoreStopwords: Boolean = false): String = {
    val cleanSentence = sentence
      .toLowerCase
      .replaceAll("[\\.]{2,}", " ") // replace multiple dots for space
      .replaceAll("(\\.|\\,)", "") // remove remaining (single) dots and commas
      .replaceAll("(\\!|\\?)", "") // remove ! and ?
      .replaceAll("[^\u0000-\uFFFF]", "") // remove emoji's ^ ^
      .replaceAll("[ ]{2,}", " ") // replace multiple spaces for single space
      .trim

    if(ignoreStopwords)
      cleanSentence
        .replaceAll(stopWords.getInPipes, "")
        .replaceAll("[ ]{2,}", " ") // replace multiple spaces for single space
        .trim
    else
      cleanSentence
  }

  def removeSentencesThatAreEmptyAfterCleanupFromReview(review: Review): Review = {
    val hasEmptySentenceAfterCleanup = review.body.exists(sentenceIsEmptyAfterCleanup(_))
    if(hasEmptySentenceAfterCleanup){
      val newBody = review.body.filterNot(sentenceIsEmptyAfterCleanup(_))
      new Review(review.id,review.date, review.rating, review.author, review.title, newBody)
    } else {
      review
    }
  }

  def sentenceIsEmptyAfterCleanup(sentence: String) = {
    cleanSentence(sentence).isEmpty
  }
}
