package domain

object stopWords {

  private val stopwords = Seq(
    "a", "an", "and", "are", "as", "at", "be", "by",
    "for", "from", "has", "he", "in", "is", "it", "its",
    "of", "on", "that", "the", "to", "was", "were", "will", "with"
  )

  def get = stopwords

  def isStopword(w: String) = stopwords.contains(w)

  def getInPipes = "(" + stopwords.mkString("|") + ")"

}
