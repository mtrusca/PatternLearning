package learning.creators

import learning.core._

class AndNodeFactory extends FunctionNodeFactory {
  override val functionType: Functions.Value = Functions.AND
  override def create: AndNode = new AndNode
}
