package learning.creators

import learning.core._

/**
  * The OrNodeFactory creates a sequence of elements
  * that should be considered a disjunction.
  */

class OrNodeFactory extends FunctionNodeFactory{
  override val functionType: Functions.Value = Functions.OR
  override def create: FunctionNode = {
    new FunctionNode {
      override val invalidFunctionChildren: Seq[Functions.Value] = Seq(Functions.OR,Functions.REPETITION, Functions.SEQUENCE)
      override val invalidTerminalChildren: Seq[Terminals.Value] = Seq(Terminals.WILDCARD)
      override val minChildren: Int = 2
      override val maxChildren: Option[Int] = None

      override val nodeType: Functions.Value = functionType
      override def hasMatch(tokens: Seq[Token]): Boolean = children.exists(_.hasMatch(tokens))
    }
  }
}
