package utilities

import learning.core.Token

case class TermStats(
  frequentWords: Seq[String],
  frequentWordPairs: Seq[(String, String)],
  frequentTagPairs: Seq[(String, String)],
  frequentWordTagPairs: Seq[(String, String)],
  frequentTagWordPairs: Seq[(String, String)]
)

class ReviewAnalyzer(labeledExamples: Seq[(Seq[Token], Int)], n: Int) {

  def getTermStats: TermStats = {
    new TermStats(
      getPositiveWords,
      getPositiveWordPairsFilteredByTopTerms, //getPositiveWordPairs,
      getPositiveTagPairs,
      getPositiveWordTagPairs,
      getPositiveTagWordPairs
    )
  }

  def getPositiveWords: Seq[String] = {
    val positives: Seq[Seq[Token]] = labeledExamples.filter(_._2 == 1).map(_._1)
    val negatives: Seq[Seq[Token]] = labeledExamples.filter(_._2 == 0).map(_._1)

    val positiveWords: Seq[String] = positives.flatMap(getSentenceInWords)
    val negativeWords: Seq[String] = negatives.flatMap(getSentenceInWords)

    val countedTopNegativesWords = getFrequencies(negativeWords).take(100).map(_._1)
    val countedAndFilteredPositiveWords = getFrequencies(positiveWords).filterNot(pair => countedTopNegativesWords.contains(pair._1))

    val selectedPairs = countedAndFilteredPositiveWords.take(n)
    selectedPairs.map(_._1)
  }

  def getPositiveWordPairs: Seq[(String, String)] = {
    val positives: Seq[Seq[Token]] = labeledExamples.filter(_._2 == 1).map(_._1)
    val negatives: Seq[Seq[Token]] = labeledExamples.filter(_._2 == 0).map(_._1)

    val positiveWordPairs: Seq[String] = positives.map(getSentenceInWords).flatMap(s => getNGrams(2, s))
    val negativeWordPairs: Seq[String] = negatives.map(getSentenceInWords).flatMap(s => getNGrams(2, s))

    val countedTopNegativesPairs = getFrequencies(negativeWordPairs).take(100).map(_._1)
    val countedAndFilteredPositivePairs = getFrequencies(positiveWordPairs).filterNot(pair => countedTopNegativesPairs.contains(pair._1))

    val selectedPairs = countedAndFilteredPositivePairs.take(n)
    selectedPairs.map(_._1).map(toPair)
  }

  def getPositiveWordPairsFilteredByTopTerms: Seq[(String, String)] = {
    val positives: Seq[Seq[Token]] = labeledExamples.filter(_._2 == 1).map(_._1)
    val negatives: Seq[Seq[Token]] = labeledExamples.filter(_._2 == 0).map(_._1)

    val positiveWords: Seq[String] = positives.map(getSentenceInWords).flatMap(s => getNGrams(1, s))
    val negativeWords: Seq[String] = negatives.map(getSentenceInWords).flatMap(s => getNGrams(1, s))

    val topNegativeWords = getFrequencies(negativeWords).take(50).map(_._1)
    val filteredTopPositiveWords = getFrequencies(positiveWords).filterNot(pair => topNegativeWords.contains(pair._1)).take(20).map(_._1)

    val bigramsWithTopTerms = positives
      .map(getSentenceInWords)
      .flatMap(s => getNGrams(2, s))
      .filter(ngram => {
        val tokens = ngram.split(" ")
        tokens.exists(filteredTopPositiveWords.contains)
      })
    val selectedPairs = getFrequencies(bigramsWithTopTerms).take(n)
    selectedPairs.map(_._1).map(toPair)
  }

  def getPositiveTagPairs: Seq[(String, String)] = {
    val positives: Seq[Seq[Token]] = labeledExamples.filter(_._2 == 1).map(_._1)
    val negatives: Seq[Seq[Token]] = labeledExamples.filter(_._2 == 0).map(_._1)

    val positiveTagPairs: Seq[String] = positives.map(getSentenceInTags).flatMap(s => getNGrams(2, s))
    val negativeTagPairs: Seq[String] = negatives.map(getSentenceInTags).flatMap(s => getNGrams(2, s))

    val countedTopNegativesPairs = getFrequencies(negativeTagPairs).take(100).map(_._1)
    val countedAndFilteredPositivePairs = getFrequencies(positiveTagPairs).filterNot(pair => countedTopNegativesPairs.contains(pair._1))

    val selectedPairs = countedAndFilteredPositivePairs.take(n)
    selectedPairs.map(_._1).map(toPair)
  }

  def getPositiveWordTagPairs: Seq[(String, String)] = {
    val positives: Seq[Seq[Token]] = labeledExamples.filter(_._2 == 1).map(_._1)
    val negatives: Seq[Seq[Token]] = labeledExamples.filter(_._2 == 0).map(_._1)

    val positiveWordPairs: Seq[String] = positives.map(getSentenceInWords).flatMap(s => getNGrams(2, s))
    val positiveTagPairs: Seq[String] = positives.map(getSentenceInTags).flatMap(s => getNGrams(2, s))
    val negativeWordPairs: Seq[String] = negatives.map(getSentenceInWords).flatMap(s => getNGrams(2, s))
    val negativeTagPairs: Seq[String] = negatives.map(getSentenceInTags).flatMap(s => getNGrams(2, s))

    val positiveTokenPairs = positiveWordPairs zip positiveTagPairs
    val negativeTokenPairs = negativeWordPairs zip negativeTagPairs

    val positiveWordTagPairs = positiveTokenPairs.map(getWordTagCombinations)
    val negativeWordTagPairs = negativeTokenPairs.map(getWordTagCombinations)

    val countedTopNegativesWordTagPairs = getFrequencies(negativeWordTagPairs).take(100).map(_._1)
    val countedAndFilteredPositiveWordTagPairs = getFrequencies(positiveWordTagPairs).filterNot(pair => countedTopNegativesWordTagPairs.contains(pair._1))

    val selectedPairs = countedAndFilteredPositiveWordTagPairs.take(n)
    selectedPairs.map(_._1).map(toPair)
  }

  def getPositiveTagWordPairs: Seq[(String, String)] = {
    val positives: Seq[Seq[Token]] = labeledExamples.filter(_._2 == 1).map(_._1)
    val negatives: Seq[Seq[Token]] = labeledExamples.filter(_._2 == 0).map(_._1)

    val positiveWordPairs: Seq[String] = positives.map(getSentenceInWords).flatMap(s => getNGrams(2, s))
    val positiveTagPairs: Seq[String] = positives.map(getSentenceInTags).flatMap(s => getNGrams(2, s))
    val negativeWordPairs: Seq[String] = negatives.map(getSentenceInWords).flatMap(s => getNGrams(2, s))
    val negativeTagPairs: Seq[String] = negatives.map(getSentenceInTags).flatMap(s => getNGrams(2, s))

    val positiveTokenPairs = positiveWordPairs zip positiveTagPairs
    val negativeTokenPairs = negativeWordPairs zip negativeTagPairs

    val positiveTagWordPairs = positiveTokenPairs.map(getTagWordCombinations)
    val negativeTagWordPairs = negativeTokenPairs.map(getTagWordCombinations)

    val countedTopNegativesTagWordPairs = getFrequencies(negativeTagWordPairs).take(100).map(_._1)
    val countedAndFilteredPositiveTagWordPairs = getFrequencies(positiveTagWordPairs).filterNot(pair => countedTopNegativesTagWordPairs.contains(pair._1))

    val selectedPairs = countedAndFilteredPositiveTagWordPairs.take(n)
    selectedPairs.map(_._1).map(toPair)
  }

  def getSentenceInWords(tokens: Seq[Token]): Seq[String] = {
    tokens.map(_.word)
  }

  def getSentenceInTags(tokens: Seq[Token]): Seq[String] = {
    tokens.map(_.tag)
  }

  def getWordTagCombinations(tokenPair: (String, String)) = {
    val words: Seq[String] = tokenPair._1.split(" ")
    val tags: Seq[String] = tokenPair._2.split(" ")
    words.head + " " + tags.last
  }

  def getTagWordCombinations(tokenPair: (String, String)) = {
    val words: Seq[String] = tokenPair._1.split(" ")
    val tags: Seq[String] = tokenPair._2.split(" ")
    tags.head + " " + words.last
  }

  def getNGrams(n: Int, elements: Seq[String]): Seq[String] = {
    elements.sliding(n).toList.map(_.mkString(" "))
  }

  def getFrequencies(elements: Seq[String]) = {
    elements
      .groupBy(w => w)
      .mapValues(_.size)
      .toList
      .sortWith(_._2 > _._2)
  }

  def toPair(stringPair: String) = {
    val words = stringPair.split(" ")
    (words.head, words.last)
  }


}
