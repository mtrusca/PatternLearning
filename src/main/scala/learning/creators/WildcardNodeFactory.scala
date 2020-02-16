package learning.creators

import learning.core.{Terminals, TerminalNodeFactory}

class WildcardNodeFactory extends TerminalNodeFactory{
  override val terminalType: Terminals.Value = Terminals.WILDCARD
  override val categories: Seq[String] = Seq(".*")
}
