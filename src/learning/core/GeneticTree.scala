package learning.core

class GeneticTree(root: FunctionNode, generationNumber: Int = 0) {

  val id = java.util.UUID.randomUUID.toString
  var generation: Int = generationNumber
  var fitness: Option[Double] = None
  var requiredTokens: Int = _

  def branches: Seq[MatchNode] = root.children

  def print() = {
    println("")
    root.children.foreach(_.print(0))
    println("")
  }

  override def toString = {
    var buffer = "\n"
    buffer = buffer + root.children.map(x => {
      val bits = x.getString(0)
      bits
    }).mkString("")
    buffer = buffer + "\n"
    buffer
  }

  def assessFitness(labeledExamples: Seq[(Seq[Token], Int)], fitnessBeta: Double) = {
    val predictions: Seq[Int] = labeledExamples map { case(sentence,label) => if(matches(sentence)) 1 else 0 }
    val answers = labeledExamples map { case(sentence, label) => label }

    val tp: Int = (predictions zip answers) count { case(predicted, actual) => (predicted == actual) && (actual == 1) }
    val fp: Int = (predictions zip answers) count { case(predicted, actual) => (predicted != actual) && (actual == 1) }
    val fn: Int = (predictions zip answers) count { case(predicted, actual) => (predicted != actual) && (actual == 0) }

    fitness = Some(fBeta(tp, fp, fn, fitnessBeta))
  }

  def matches(tokens: Seq[Token]): Boolean = {
    val firstBranch = root.children.head
    val requiredTokensForMatch = getRequiredTokenSizesPerNode.sum
    val entryPoints = getEntryPoints(firstBranch, tokens, requiredTokensForMatch)
    val hasEntryPoint = entryPoints.nonEmpty

    if(hasEntryPoint) {
      entryPoints.exists(e => hasMatchForEntryPoint(e, tokens))
    } else {
      false
    }
  }

  def hasMatchForEntryPoint(entryPoint: Int, tokens: Seq[Token]): Boolean = {
    val partitionSizes = getRequiredTokenSizesPerNode
    val tokenPartitions = getTokenPartitionsFromEntryPoint(partitionSizes, tokens, entryPoint)
    val toMatchPairs = root.children zip tokenPartitions
    val isMatch = toMatchPairs forall { case(node: MatchNode, partition :Seq[Token]) => node.hasMatch(partition)}
    isMatch
  }

  def getTokenPartitionsFromEntryPoint(requiredPartitionSizes: Seq[Int], tokens: Seq[Token], entryPoint: Int) = {
    var i = entryPoint
    requiredPartitionSizes.map(size => {
      val from = i
      val until = from + size
      i = i + size
      tokens.slice(from, until)
    })
  }

  def getRequiredTokenSizesPerNode: Seq[Int] = {
    root.children.map(child => {
      child match {
        case node: SequenceNode => node.children.size
        case _ => 1
      }
    })
  }

  def getEntryPoints(node: MatchNode, tokens: Seq[Token], nodeSpan: Int): Seq[Int] = {
    val patternFits = nodeSpan <= tokens.size
    if(patternFits){
      val maxIndex = tokens.size - nodeSpan
      node match {
        case node: SequenceNode =>
          tokens.indices.filter(index => {
            if(index <= maxIndex) {
              val tokensForMatch = tokens.slice(index, index + nodeSpan)
              node.hasMatch(tokensForMatch)
            } else {
              false
            }
          })
        case _ => tokens.zipWithIndex.filter(el => {
          val index = el._2
          val token = el._1
          if(index <= maxIndex) {
            node.hasMatch(Seq(token))
          } else {
            false
          }
        }).map(_._2)
      }

    } else {
      Seq()
    }
  }

  def fBeta(truePositives: Int, falsePositives: Int, falseNegatives: Int, beta: Double): Double = {
    val isComputable = truePositives != 0
    if(isComputable) {
      val precision = truePositives.toDouble / (truePositives + falsePositives).toDouble
      val recall = truePositives.toDouble / (truePositives + falseNegatives).toDouble
      val betaSquared = beta * beta
      ( (1.0 + betaSquared) * (precision * recall) ) / ( betaSquared * precision + recall )
    } else {
      0.0
    }
  }

  override def clone: GeneticTree = {
    new GeneticTree(root, generation + 1)
  }

//  def getAllNodesFromRoot: Seq[MatchNode] = {
//    root.getChildren()
//  }

}
