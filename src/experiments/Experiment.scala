package experiments

import java.io._

import domain.{Evaluator, FeedbackTypes}
import utilities.BowFeaturizer

abstract class Experiment() {
  val evaluator = new Evaluator
  val name: String
  val target: String

  var performance: Performance

  def run

  def getReport = {
      """
        |Method: %s
        |Target: %s
        |Performance scores:
        |=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        |Precision: %s
        |Recall: %s
        |F1-score: %s
        |
        |
      """.stripMargin.format(name, target, performance.precision, performance.recall, performance.f1)
  }

}
