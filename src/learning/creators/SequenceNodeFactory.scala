package learning.creators

import learning.core._

class SequenceNodeFactory extends FunctionNodeFactory {
  override val functionType: Functions.Value = Functions.SEQUENCE
  override def create(): FunctionNode = new SequenceNode
}
