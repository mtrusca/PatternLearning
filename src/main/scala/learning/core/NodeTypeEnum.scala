package learning.core

object Functions extends Enumeration {
  val SEQUENCE, AND, OR, NOT, REPETITION = Value
}

object Terminals extends Enumeration {
  val POS, CONCEPT, WILDCARD, NEGATION, LITERAL = Value
}

object RepetitionType extends Enumeration {
  val ZEROORMORE, ONEORMORE, ZEROORONE = Value
}
