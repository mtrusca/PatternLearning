package learning.core

import scala.util.Random

trait NodeFactory {
  def create(): MatchNode
}

trait FunctionNodeFactory extends NodeFactory {
  val functionType: Functions.Value
  override def create(): FunctionNode
}

trait TerminalNodeFactory extends NodeFactory {
  val categories: Seq[String]
  val terminalType: Terminals.Value

  override def create(): TerminalNode = {
    val pattern: String = {
      Random.shuffle(categories).head
    }
    new TerminalNode(terminalType, pattern)
  }
}