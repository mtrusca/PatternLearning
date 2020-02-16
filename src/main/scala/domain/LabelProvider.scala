package domain

abstract class LabelProvider {

  def getLabelsForTask(reviews: Seq[Review], task: (String) => Boolean): Seq[Int] = {
    val allSentences = reviews.flatMap(_.body)
    val allLabels = allSentences.map(task(_))
    allLabels.map(boolToInt)
  }

  def boolToInt(bool: Boolean) = if(bool) 1 else 0
}
