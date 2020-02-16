package experiments

import domain.{Review, FeedbackTypes}
import learning.GPMethod
import learning.core.{Terminals, TerminalNode, GeneticTree, PatternGroup}
import learning.creators.SequenceNodeFactory
import org.apache.spark.SparkContext
import usecases.{NoisyLabelProviderFromComplexPatterns, GoldenLabelProvider}
import utilities.DataSource

/**
  * Created by Gino on 29/02/16.
  */

class ExperimentManualTreePatterns(
targetFeedbackType: FeedbackTypes.Value,
sc: SparkContext,
db: DataSource) extends Experiment {

  override val name: String = "Manually created Tree Patterns"
  override var performance: Performance = _
  override val target: String = targetFeedbackType.toString
  override def run: Unit = {

    val patternGroup: PatternGroup = new PatternGroup(targetFeedbackType)
    patternGroup.patterns = Seq(
      constructTreeFromSequence(List("it", "VB", "(n't|not)")),
      constructTreeFromSequence(List("(ca|can)", "(n't|not)")),
      constructTreeFromSequence(List("(crash|freeze|bug|problem)")),
      constructTreeFromSequence(List("(however|but)")),
      constructTreeFromSequence(List("no", "(option|ability)", "to")),
      constructTreeFromSequence(List("(update|updates)"))
    )
    val noisyLabelProvider = new NoisyLabelProviderFromComplexPatterns(patternGroup)
    val testReviews = db.getTestReviews

    val goldenLabelProvider = new GoldenLabelProvider(db.getReviewAnnotations)
    val testLabels = targetFeedbackType match {
      case FeedbackTypes.DEFECT_REPORT => goldenLabelProvider.getDefectLabelsForReviews(testReviews)
      case FeedbackTypes.IMPROVEMENT_REQUEST => goldenLabelProvider.getImprovementLabelsForReviews(testReviews)
    }

    val predictedLabels = noisyLabelProvider.getReviewLabels(testReviews)
    val scoreAndLabels: Seq[(Int, Int)] = predictedLabels zip testLabels
    performance = evaluator.getPerformance(scoreAndLabels)

  }

  def constructTreeFromSequence(literals: Seq[String]) = {
    val root = (new SequenceNodeFactory).create
    root.children = literals.map(l => {
      new TerminalNode(Terminals.LITERAL, l)
    })
    new GeneticTree(root)
  }

}
