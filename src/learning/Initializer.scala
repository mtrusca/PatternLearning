package learning

import learning.core._
import learning.creators._
import utilities.TermStats


class Initializer(functions: Seq[FunctionNodeFactory], terminals: Seq[TerminalNodeFactory], stats: TermStats ) {

  val maxBranchesInTree = 3 // the max number of branches generated from the root
  val maxDepth = 2
  val maxChildren = 3
  val randomizer = scala.util.Random

  def getRandomNumber(min: Int, max: Int) = randomizer.nextInt(max) + min

  def newGeneticTree: GeneticTree = {
    val branches = randomizer.nextInt(maxBranchesInTree) + 1
    val root = (new SequenceNodeFactory).create
    root.children = (1 to branches).map(x => generateRandomTree(maxDepth))
    new GeneticTree(root)
  }

  def generateRandomTree(
    maxDepth: Int,
    invalidFunctions: Seq[Functions.Value] = Seq(),
    invalidTerminals: Seq[Terminals.Value] = Seq()
  ): MatchNode = {
    val validFunctions = functions.filterNot(f => invalidFunctions.contains(f.functionType))
    val validTerminals = terminals.filterNot(t => invalidTerminals.contains(t.terminalType))

    if(maxDepth == 0){
      randomizer.shuffle(validTerminals).head.create
    } else {
      val nextNode = randomizer.shuffle(validTerminals ++ validFunctions).head.create
      nextNode match {
        case tNode: TerminalNode => tNode
        case fNode: FunctionNode =>
          val min = fNode.minChildren
          val max =  fNode.maxChildren.getOrElse(maxChildren)
          val nrOfChildren = if(max == 1) 1 else getRandomNumber(min, max)

          if(fNode.nodeType == Functions.SEQUENCE) {
            val randomPairType = getRandomNumber(1,4)
            val children: (TerminalNode, TerminalNode) = composeChildrenFromSelectedPairType(randomPairType)
            fNode.children = Seq(children._1, children._2)
          } else {
            fNode.children = (1 to nrOfChildren).map(x => {
              generateRandomTree(maxDepth - 1, fNode.invalidFunctionChildren, fNode.invalidTerminalChildren)
            })
          }

          fNode
        case _ => throw new IllegalStateException("Error while constructing tree..")
      }
    }
  }

  def composeChildrenFromSelectedPairType(pairType: Int): (TerminalNode, TerminalNode) = {

//    val pair = randomizer.shuffle(stats.frequentWordPairs).head
//    val termOne = pair._1
//    val termTwo = pair._2
//    (new TerminalNode(Terminals.LITERAL, termOne), new TerminalNode(Terminals.LITERAL, termTwo))

    pairType match {
      case 1 => {
        val pair = randomizer.shuffle(stats.frequentWordPairs).head
        val termOne = pair._1
        val termTwo = pair._2
        (new TerminalNode(Terminals.LITERAL, termOne), new TerminalNode(Terminals.LITERAL, termTwo))
      }
      case 2 => {
        val pair = randomizer.shuffle(stats.frequentWordTagPairs).head
        val termOne = pair._1
        val termTwo = pair._2
        (new TerminalNode(Terminals.LITERAL, termOne), new TerminalNode(Terminals.POS, termTwo))
      }
      case 3 => {
        val pair = randomizer.shuffle(stats.frequentTagPairs).head
        val termOne = pair._1
        val termTwo = pair._2
        (new TerminalNode(Terminals.POS, termOne), new TerminalNode(Terminals.POS, termTwo))
      }
      case 4 => {
        val pair = randomizer.shuffle(stats.frequentTagWordPairs).head
        val termOne = pair._1
        val termTwo = pair._2
        (new TerminalNode(Terminals.POS, termOne), new TerminalNode(Terminals.LITERAL, termTwo))
      }
    }


  }

}
