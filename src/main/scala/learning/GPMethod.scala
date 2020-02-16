package learning


import domain._
import experiments.GPConfiguration
import learning.core._
import learning.creators._
import utilities.ReviewAnalyzer

class GPMethod(
  config: GPConfiguration,
  trainingReviews: Seq[Review],
  goldenLabels: Seq[Int],
  targetType: FeedbackTypes.Value,
  reviewLevel: Boolean = false) {

  def runGPMethodTimed(): PatternGroup = {
    time { runGPMethod() }
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    val totalSeconds = (t1 - t0) / 1000000000.0
    val minutes: Int = (totalSeconds / 60.0).round.toInt
    val seconds = (totalSeconds % 60.0).round.toInt
    println("Elapsed time: " + minutes + " minutes and " + seconds + " seconds")
    result
  }

  def runGPMethod(): PatternGroup = {

    val learningExamples = getLearningExamples(trainingReviews, goldenLabels)
    val stats = new ReviewAnalyzer(learningExamples, 50).getTermStats

    val functions: Seq[FunctionNodeFactory] = Seq(
      new AndNodeFactory,
      new OrNodeFactory,
      new NotNodeFactory,
      new RepetitionNodeFactory,
      new SequenceNodeFactory
    )

    val terminals: Seq[TerminalNodeFactory] = Seq(
      new LiteralNodeFactory(stats.frequentWords),
      new SyntacticalNodeFactory,
      new WildcardNodeFactory,
      new ConceptNodeFactory
    )

    val initializer = new Initializer(functions, terminals, stats)
    val initialPopulation = (1 to config.populationSize) map { x => initializer.newGeneticTree }


    val selector = new TournamentSelector(
      config.populationSize,
      config.tournamentSelectionRatio,
      config.fitnessBeta
    )

    val evolver = new Evolver(
      selector,
      config.populationSize,
      config.maxGenerations,
      config.reproductionRatio,
      config.crossoverRatio,
      config.mutationRatio,
      config.fitnessBeta,
      initializer
    )

    val finalPatternGroup = searchPatterns(initialPopulation, learningExamples, evolver)
    finalPatternGroup
  }

  def searchPatterns(
    population: Seq[GeneticTree], learningExamples: Seq[(Seq[Token], Int)], evolver: Evolver,
    patternGroup: PatternGroup = new PatternGroup(targetType)): PatternGroup = {
      if(patternGroup.isSatisfied) {
        patternGroup
      } else {
        val newBestPattern = searchBestPatternForGivenExamples(population, learningExamples, evolver)
        val newLearningExamples = removePositiveExamplesForGivenPattern(newBestPattern, learningExamples)
        patternGroup.addPatternIfGainful(newBestPattern, learningExamples)
        searchPatterns(population, newLearningExamples, evolver, patternGroup)
      }
  }

  def removePositiveExamplesForGivenPattern(pattern: GeneticTree, learningExamples: Seq[(Seq[Token], Int)]) = {
    learningExamples filterNot { case(sentence, label) => pattern.matches(sentence) }
  }

  def removeTruePositiveExamplesForGivenPattern(pattern: GeneticTree, learningExamples: Seq[(Seq[Token], Int)]) = {
    learningExamples filterNot { case(sentence, label) => pattern.matches(sentence) && label == 1 }
  }

  def searchBestPatternForGivenExamples(
    population: Seq[GeneticTree],
    learningExamples: Seq[(Seq[Token], Int)],
    evolver: Evolver) = {
    val finalPopulation = evolver.run(population, learningExamples)
    val rankedPopulation = finalPopulation.sortWith(_.fitness.get > _.fitness.get)
    rankedPopulation.head
  }


  def getLearningExamples(trainingReviews: Seq[Review], goldenLabels: Seq[Int]) = {
    if(reviewLevel){
      val learningSentences = trainingReviews.map(_.body.mkString(" ")).map(s => {
        val sentence = Parser.parseSentence(s)
        val tokens = (sentence.words zip sentence.tags.get) map { case(word, tag) => new Token(word, tag) }
        tokens.toSeq
      })

      learningSentences zip goldenLabels
    } else {
      val learningSentences = trainingReviews.flatMap(_.body).map(s => {
        val sentence = Parser.parseSentence(s)
        val tokens = (sentence.words zip sentence.tags.get) map { case(word, tag) => new Token(word, tag) }
        tokens.toSeq
      })

      learningSentences zip goldenLabels
    }

  }

}
