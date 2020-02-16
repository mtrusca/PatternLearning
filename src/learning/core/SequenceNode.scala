package learning.core

class SequenceNode extends FunctionNode {
  override val nodeType: Functions.Value = Functions.SEQUENCE
  override val minChildren: Int = 2
  override val maxChildren: Option[Int] = None
  override val invalidFunctionChildren: Seq[Functions.Value] = Seq(
    Functions.REPETITION,
    Functions.SEQUENCE,
    Functions.NOT
  )
  override val invalidTerminalChildren: Seq[Terminals.Value] = Seq()

  override def hasMatch(tokens: Seq[Token]): Boolean = {
    if(tokens.size == children.size){
      (tokens zip children).forall(pair => pair._2.hasMatch(Seq(pair._1)))
    } else {
      throw new IllegalStateException("Unable to match sequence, for the provided input!")
    }
  }
}
