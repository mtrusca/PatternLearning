package domain

import learning.core._
import learning.creators.{OrNodeFactory, SequenceNodeFactory}

/**
  * The Generated Patterns (Patterns B in Report)
  * These patterns have been selected from the Learning Engine output
  */

object TreePatterns {

  val defectPatternGroup: PatternGroup = new PatternGroup(FeedbackTypes.DEFECT_REPORT)
  defectPatternGroup.patterns = Seq(
    constructTreeFromOR(Seq("(however|but)", "(not|n't)")),
    constructTreeFromOR(Seq("(bug|bugs|buggy|crash|crashes|crashing|error|problem|fix|issue|issues|freeze|freezes|fail|failed|failing)", "(update|updates)"))
  )

  val improvementPatternGroup: PatternGroup = new PatternGroup(FeedbackTypes.IMPROVEMENT_REQUEST)
  improvementPatternGroup.patterns = Seq(
    constructTreeFromSequence(Seq("5", "stars")),
    constructTreeFromSequence(Seq("please", "(VB|VBS)")),
    constructTreeFromSequence(Seq("(i|it|that|they|you)", "(would)")),
    constructTreeFromSequence(Seq("(would|should)", "be")),
    constructTreeFromSequence(Seq("(just|only|i)", "(wish|request|recommend)")),
    constructTreeFromSequence(Seq("wish", "(it|the|that|there|they|dropbox|ebay|evernote|mint|todoist|fitbit)")),
    constructTreeFromSequence(Seq("only", "thing")),
    constructTreeFromSequence(Seq("the", "only")),
    constructTreeFromOR(Seq("if", "just"))
  )


  def constructTreeFromSequence(literals: Seq[String]) = {
    val root = (new SequenceNodeFactory).create()
    val node = (new SequenceNodeFactory).create()
    node.children = literals.map(l => {
      new TerminalNode(Terminals.LITERAL, l)
    })
    root.children = Seq(node)
    new GeneticTree(root)
  }

  def constructTreeFromOR(literals: Seq[String]): GeneticTree = {
    val root = (new SequenceNodeFactory).create()
    val node = (new OrNodeFactory).create()
    node.children = literals.map(l => {
      new TerminalNode(Terminals.LITERAL, l)
    })
    root.children = Seq(node)
    new GeneticTree(root)
  }

  def constructTreeFromLiteral(literal: String): GeneticTree = {
    val root = (new SequenceNodeFactory).create()
    root.children = Seq(new TerminalNode(Terminals.LITERAL, literal))
    new GeneticTree(root)
  }
}
