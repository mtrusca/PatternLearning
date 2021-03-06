package learning.creators

import learning.core._


class NotNodeFactory extends FunctionNodeFactory {
  override val functionType: Functions.Value = Functions.NOT
  override def create: FunctionNode = {
    new FunctionNode {
      override val invalidFunctionChildren: Seq[Functions.Value] = Seq(Functions.NOT,Functions.REPETITION, Functions.SEQUENCE)
      override val invalidTerminalChildren: Seq[Terminals.Value] = Seq(Terminals.WILDCARD)
      override val minChildren: Int = 1
      override val maxChildren: Option[Int] = Some(1)

      override val nodeType: Functions.Value = functionType
      override def hasMatch(tokens: Seq[Token]): Boolean = !children.exists(_.hasMatch(tokens))

    }
  }
}
