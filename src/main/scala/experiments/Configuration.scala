package experiments

trait Configuration {

}

case class GPConfiguration(
  populationSize: Int, // best found = 500
  maxGenerations: Int, // best found = 100
  fitnessBeta: Double, // best found = 0.2. A beta smaller than 1.0, results in more weight on precision (rather then recall)
  tournamentSelectionRatio: Double, // best found = 0.1
  reproductionRatio: Double, // best found = 0.05
  crossoverRatio: Double, // best found = 0.45
  mutationRatio: Double // best found = 0.5
) extends Configuration

