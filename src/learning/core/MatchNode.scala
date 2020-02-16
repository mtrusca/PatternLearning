package learning.core

trait MatchNode {
  def hasMatch(tokens: Seq[Token]): Boolean
  def print(depth: Int)
  def getString(depth: Int): String
}

trait FunctionNode extends MatchNode {
  val nodeType: Functions.Value
  val invalidFunctionChildren: Seq[Functions.Value]
  val invalidTerminalChildren: Seq[Terminals.Value]
  val minChildren: Int
  val maxChildren: Option[Int]
  var children: Seq[MatchNode] = Seq()

  override def print(depth: Int) = {
    depth match {
      case 0 =>
        println("*-" + nodeType + ": ")
      case _ =>
        println("|" + ("  " * depth) + "*-" + nodeType + ": ")
    }
    children.foreach(_.print(depth + 1))
  }

  def getString(depth: Int) = {
    var buffer = ""
    depth match {
      case 0 =>
        buffer = buffer + "\n*-" + nodeType + ": "
      case _ =>
        buffer = buffer + "\n|" + ("  " * depth) + "*-" + nodeType + ": "
    }
    buffer = buffer + children.map(_.getString(depth + 1)).mkString("")
    buffer
  }

}

trait RepetitionFunctionNode extends FunctionNode {
  val repetitionType: RepetitionType.Value

  override def print(depth: Int) = {
    depth match {
      case 0 =>
        println("*-" + "%s(%s): ".format(nodeType, repetitionType))
      case _ =>
        println("|" + ("  " * depth) + "*-" + "%s(%s): ".format(nodeType, repetitionType))
    }
    children.foreach(_.print(depth + 1))
  }

  override def getString(depth: Int) = {
    var buffer = ""
    depth match {
      case 0 =>
        buffer = buffer + "\n*-" + "%s(%s): ".format(nodeType, repetitionType)
      case _ =>
        buffer = buffer + "\n|" + ("  " * depth) + "*-" + "%s(%s): ".format(nodeType, repetitionType)
    }
    buffer = buffer + children.foreach(_.getString(depth + 1))
    buffer
  }
}

class TerminalNode(nodeType: Terminals.Value, pattern: String) extends MatchNode {

  def hasMatch(tokens: Seq[Token]): Boolean = {
    if(tokens.nonEmpty){
      tokens.head.word.matches(pattern) || tokens.head.tag.matches(pattern)
    } else {
      throw new IllegalStateException("no tokens to match!")
    }

  }

  override def print(depth: Int) = depth match {
    case 0 =>
      println("*-" + pattern)
    case _ =>
      println("|" + ("  " * depth) + "*-" + pattern)
  }

  def getString(depth: Int) = {
    var buffer = ""
    depth match {
      case 0 =>
        buffer = buffer + "\n*-" + pattern
      case _ =>
        buffer = buffer + "\n|" + ("  " * depth) + "*-" + pattern
    }
    buffer
  }
}
