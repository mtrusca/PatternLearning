package utilities

import java.io._

import domain.Review

class DataPreparator extends Serializable {

  val bowFeaturizer = new BowFeaturizer

  def writeDataPointsToFile(
    labels: Seq[Int],
    sparseMatrix: Seq[Seq[(Int, Int)]],
    fileName: String,
    path: String = "data/mllib/") = {

    val allRowsHaveLabels = labels.size == sparseMatrix.size
    if(allRowsHaveLabels) {
      val rows = labels.indices.map(i => {
        val label = labels(i)
        val features = sparseMatrix(i)
        getLibSVMFormattedDataPoint(label, features)
      })
      val pw = new PrintWriter(path + new File(fileName))
      rows.foreach(row => pw.write(row + "\n"))
      pw.close
    } else {
      throw new IllegalStateException("Oops! Not all data rows have a corresponding label")
    }
  }

  def getLibSVMFormattedDataPoint(label: Int, sparseVector: Seq[(Int, Int)]): String = {
    label + " " + sparseVector.map(getLibSVMFeatureFromTuple(_)).mkString(" ")
  }

  def getLibSVMFeatureFromTuple(tuple: (Int, Int)) = {
    tuple._1.toString + ":" + tuple._2.toString
  }

  def getSparseMatrixInLibSVMFormatReviewLevel(reviews: Seq[Review], bowIndices: Seq[String], useNGrams: Boolean = false): Seq[Seq[(Int,Int)]] = {
    getSparseMatrixForReviews(reviews, bowIndices, useNGrams).map(getOneBasedVector)
  }

  def getSparseMatrixInLibSVMFormatSentenceLevel(reviews: Seq[Review], bowIndices: Seq[String], useNGrams: Boolean = false): Seq[Seq[(Int,Int)]] = {
    getSparseMatrixForSentences(reviews, bowIndices, useNGrams).map(getOneBasedVector)
  }

  def getSparseMatrixForReviews(reviews: Seq[Review], bowIndices: Seq[String], useNGrams: Boolean = false): Seq[Seq[(Int,Int)]] = {
    reviews.map(_.body.mkString(" "))
      .map(getSentenceSparseVector(_, bowIndices, useNGrams))
  }

  def getSparseMatrixForSentences(reviews: Seq[Review], bowIndices: Seq[String], useNGrams: Boolean = false): Seq[Seq[(Int,Int)]] = {
    reviews.flatMap(_.body)
      .map(getSentenceSparseVector(_,bowIndices, useNGrams))
  }

  def getSentenceSparseVector(sentence: String, bowIndices: Seq[String], useNGrams: Boolean = false): Seq[(Int,Int)] = {

    val tokens = if(useNGrams) {
      val n = estimateNGramSize(bowIndices.head) // ugly, please fix!
      bowFeaturizer.getNGrams(n, sentence)
    } else {
      bowFeaturizer.getTokens(sentence)
    }

    tokens
      .distinct
      .map(token => {
        val index = bowIndices.indexOf(token)
        val count = tokens.count(_.equals(token))
        (index,count)
      })
      .filter(_._1 > 0) // required in order to ignore unindexed words (words in test data, that where not in training data)
      .sortBy(_._1)
  }

  def estimateNGramSize(bowIndex: String) = bowIndex.split(" ").size

  def getOneBasedVector(vector: Seq[(Int, Int)]) = {
    vector.map(pair => ( (pair._1 + 1),pair._2))
  }
}
