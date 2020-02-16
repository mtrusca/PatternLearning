package learning.core

class AndNode extends FunctionNode {
  override val invalidFunctionChildren: Seq[Functions.Value] = Seq(Functions.AND, Functions.REPETITION, Functions.SEQUENCE)
  override val invalidTerminalChildren: Seq[Terminals.Value] = Seq(Terminals.WILDCARD)
  override val minChildren: Int = 2
  override val maxChildren: Option[Int] = None
  override val nodeType: Functions.Value = Functions.AND
  override def hasMatch(tokens: Seq[Token]): Boolean = children.forall(_.hasMatch(tokens))
}
