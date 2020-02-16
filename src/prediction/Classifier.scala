package prediction

import domain.Review
import org.apache.spark.mllib.classification.SVMModel
import org.apache.spark.rdd.RDD

trait Classifier {

  val name: String

  def train(trainingDocs: Seq[Review], trainingLabels: Seq[Int]): SVMModel

  def predict(testDocs: Seq[Review], testLabels: Seq[Int], model: SVMModel): RDD[(Double, Double)]

}
