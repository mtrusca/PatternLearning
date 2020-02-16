package learning.creators

import learning.core.{Terminals, TerminalNodeFactory}

class SyntacticalNodeFactory extends TerminalNodeFactory {
  override val terminalType: Terminals.Value = Terminals.POS
  override val categories: Seq[String] = Seq(
    "CC", "CD", "DT", "EX", "FW", "IN", "JJ", "JJR", "JJS", "LS", "MD", "NN",
    "NNS", "NNP", "NNPS", "PDT", "POS", "PRP", "PRP$", "RB", "RBR", "RBS", "RP", "SYM", "TO", "UH", "VB",
    "VBD", "VBG", "VBN", "VBP", "VBZ", "WDT", "WP", "WP$", "WRB"
  )
}

