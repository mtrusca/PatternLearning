package learning.core

import domain.{FeedbackTypes, Evaluator}
import java.io._

class PatternGroup(feedbackType: FeedbackTypes.Value) {

  val eval = new Evaluator
  var accuracy = 0.0
  var patterns: Seq[GeneticTree] = Seq()
  var isSatisfied = false
  val maxGenerationSameFitness = 3
  var nrOfGenerationsNoFitnessGain = 0


  def addPatternIfGainful(newPattern: GeneticTree, learningExamples: Seq[(Seq[Token], Int)]) = {
    val newPatternGroup = patterns ++ Seq(newPattern)
    val newAccuracy = computeAccuracy(newPatternGroup, learningExamples)
    if(newAccuracy > accuracy) {
      patterns = newPatternGroup
      accuracy = newAccuracy
      if(nrOfGenerationsNoFitnessGain > 0)
        nrOfGenerationsNoFitnessGain = 0
    } else {
      nrOfGenerationsNoFitnessGain = nrOfGenerationsNoFitnessGain + 1
      if(nrOfGenerationsNoFitnessGain > maxGenerationSameFitness){
        isSatisfied = true
      }
    }
  }

  def computeAccuracy(providedPatterns: Seq[GeneticTree], labelledExamples: Seq[(Seq[Token], Int)] ): Double = {
    val predictions = labelledExamples.map(d => if(providedPatterns.exists(_.matches(d._1))) 1 else 0)
    val answers = labelledExamples.map(_._2)
    eval.getAccuracy(predictions, answers)
  }

  def storePatternsToFile() = {
    val timestamp: Long = System.currentTimeMillis / 1000
    val printablePatterns = patterns.map(_.toString).mkString("")
    val fileName = feedbackType match {
      case FeedbackTypes.DEFECT_REPORT => "defect_patterns_" + timestamp
      case FeedbackTypes.IMPROVEMENT_REQUEST => "improvement_patterns_" + timestamp
    }

    val file = new File("data/patterns/%s.txt".format(fileName))
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(printablePatterns)
    bw.close()
  }

  def getPatternsInString = {
    patterns.map(_.toString).mkString("")
  }

}
