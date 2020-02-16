package learning.creators

import learning.core._
import scala.util.Random

class RepetitionNodeFactory extends FunctionNodeFactory {

  override val functionType: Functions.Value = Functions.REPETITION

  override def create: FunctionNode = {
    new RepetitionFunctionNode {
      override val nodeType: Functions.Value = Functions.REPETITION
      override val repetitionType: RepetitionType.Value = getRandomRepetitionType
      override val invalidFunctionChildren: Seq[Functions.Value] = Seq(
        Functions.REPETITION,
        Functions.SEQUENCE,
        Functions.NOT
      )
      override val invalidTerminalChildren: Seq[Terminals.Value] = Seq(
        Terminals.NEGATION,
        Terminals.WILDCARD
      )
      override val minChildren: Int = 1
      override val maxChildren: Option[Int] = Some(1)
      override def hasMatch(tokens: Seq[Token]): Boolean = {
        val child = children.head
        repetitionType match {
          case RepetitionType.ONEORMORE =>
            tokens.take(2).forall(t => child.hasMatch(Seq(t))) || // one repetition
            tokens.take(3).forall(t => child.hasMatch(Seq(t)))   // more repetition / at least two repetitions
          case RepetitionType.ZEROORMORE =>
            tokens.take(2).forall(t => !child.hasMatch(Seq(t))) || // zero repetition
            tokens.take(3).forall(t => child.hasMatch(Seq(t)))    // more repetition / at least two repetitions
          case RepetitionType.ZEROORONE =>
            tokens.take(2).forall(t => !child.hasMatch(Seq(t))) || // zero repetition
            tokens.take(2).forall(t => child.hasMatch(Seq(t)))    // one repetition
        }
      }
    }
  }


  def getRandomRepetitionType: RepetitionType.Value = {
    Random.shuffle(
      Seq(
        RepetitionType.ONEORMORE,
        RepetitionType.ZEROORMORE,
        RepetitionType.ZEROORONE
      )
    ).head
  }
}

