package learning.creators

import learning.core.{Terminals, TerminalNodeFactory}

class NegationNodeFactory extends TerminalNodeFactory {
  override val terminalType: Terminals.Value = Terminals.NEGATION
  override val categories: Seq[String] = Seq("(not|n't)", "(however|but)")
}