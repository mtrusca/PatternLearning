package domain


// The Manual Patterns (Patterns A in Report)

object BasicPatterns {

  private val NEG = "([a-z]+n't|not|no)" // Negation
  private val USER = "(me|myself|i|us|we|ourselves|ourself|you)" // Notion of the user(s) (of the app)
  private val COMPANY = "(dropbox|ebay|evernote|mint|todoist|fitbit|the makers)" // Notion of the company / provider


  val defectPatterns = List(
    List("VB", "(n't|not)"),
    List("i", "(ca|can)", "(n't|not)"),
    List("(crash|freeze|bug|problem)"),
    List("(however|but)"),
    List("no", "(option|ability)", "to"),
    List("(update|updates)")
  )

  val improvementPatterns = List(
      List("please", "VB"),
      List("5", "stars"),
      List("i", "ca", "n't"),
      List("(an|the)", "option", "to"),
      List("i", "(wish|need)"),
      List("would", "VB"),
      List("it", "should"),
      List("there", "should"),
      List("(however|but)"),
      List("if")
  )
}
