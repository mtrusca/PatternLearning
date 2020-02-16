package learning.creators

import learning.core.{Terminals, TerminalNodeFactory}

class LiteralNodeFactory(frequentTerms: Seq[String]) extends TerminalNodeFactory {
  override val terminalType: Terminals.Value = Terminals.LITERAL
  override val categories: Seq[String] = frequentTerms
//    Seq(
//    "latest", "problem", "however", "issues", "whenever", "since",
//    "desktop", "app", "last", "some", "will", "note", "version",
//    "wanted", "delete", "problem", "mobile", "client", "few",
//    "issues", "even", "battery", "support", "conflict"
//  )
}
