package learning

import learning.core.GeneticTree

class TournamentSelector(populationSize: Int, tournamentSelectionRatio: Double, fitnessBeta: Double) {

  val tournamentSize = (populationSize * tournamentSelectionRatio).round.toInt
  val randomizer = scala.util.Random

  def selectIndividual(population: Seq[GeneticTree]): GeneticTree = {
    val competitors = randomizer.shuffle(population).take(tournamentSize)
    getWinner(competitors)
  }

  def selectIndividuals(population: Seq[GeneticTree], amount: Int): Seq[GeneticTree] = {
    if(amount <= tournamentSize) {
      val competitors = randomizer.shuffle(population).take(tournamentSize)
      getWinners(competitors, amount)
    } else {
      throw new IllegalArgumentException("selection size is bigger then tournament size !")
    }
  }

  def getWinner(competitors: Seq[GeneticTree]): GeneticTree = {
    competitors
      .sortWith(_.fitness.get > _.fitness.get)
      .head
  }

  def getWinners(competitors: Seq[GeneticTree], amount: Int): Seq[GeneticTree] = {
    competitors
      .sortWith(_.fitness.get > _.fitness.get)
      .take(amount)
  }
}
