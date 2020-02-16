package learning

import learning.core.{Token, MatchNode, GeneticTree}
import learning.creators.SequenceNodeFactory

import scala.util.Random

case class Position(depth: Int, index: Int)
case class GeneticMaterial(self: GeneticTree, targetNode: MatchNode, targetNodePosition: Position)
case class Parents(mother: GeneticMaterial, father: GeneticMaterial)

class Evolver(
    selector: TournamentSelector,
    populationSize: Int,
    maxGenerations: Int,
    reproductionRatio: Double,
    crossoverRatio: Double,
    mutationRatio: Double,
    fitnessBeta: Double,
    initializer: Initializer
) {

  def run(population: Seq[GeneticTree], challenges: Seq[(Seq[Token], Int)]): Seq[GeneticTree] = {
    population.foreach(_.assessFitness(challenges, fitnessBeta))
    val isLastGeneration = population.head.generation == maxGenerations
    if(isLastGeneration) {
      population
    } else {
      val reproductionOffspring = getReproductionOffspring(population, reproductionRatio)
      val crossoverOffspring = getCrossoverOffspring(population, crossoverRatio)
      val mutationOffspring = getMutationOffspring(population, mutationRatio)
      val nextGeneration = reproductionOffspring ++ crossoverOffspring ++ mutationOffspring
      run(nextGeneration, challenges)
    }
  }

  def getReproductionOffspring(population: Seq[GeneticTree], reproductionRatio: Double) = {
    val reproductionSize = (population.size * reproductionRatio).round.toInt
    val elite = selector.selectIndividuals(population, reproductionSize)
    elite.map(_.clone)
  }

  def getCrossoverOffspring(population: Seq[GeneticTree], crossoverRatio: Double): Seq[GeneticTree] = {
    val crossoverSize = (population.size * crossoverRatio).round.toInt / 2 // divided by two, because it generates 2 parents
    (1 to crossoverSize).flatMap(x => {
      val candidateParents = selector.selectIndividuals(population, 2)
      val parents = getParents(candidateParents)
      val mother = parents.mother
      val father = parents.father
      val offspringOne = createTreeFromProvidedMaterials(father, mother.targetNode)
      val offspringTwo = createTreeFromProvidedMaterials(mother, father.targetNode)
      Seq(offspringOne, offspringTwo)
    })
  }

  def getMutationOffspring(population: Seq[GeneticTree], mutationRatio: Double): Seq[GeneticTree] = {
    val mutationSize = (population.size * mutationRatio).round.toInt
    (1 to mutationSize).map(x => {
      val parent = selector.selectIndividual(population)
      val targetNodePosition = selectRandomNodePositionFromTree(parent)
      val targetNode = parent.branches(targetNodePosition.index)
      val nodeToInject = initializer.generateRandomTree(initializer.maxDepth)
      val geneticMaterialParent = new GeneticMaterial(parent, targetNode, targetNodePosition)
      createTreeFromProvidedMaterials(geneticMaterialParent, nodeToInject)
    })
  }

  def getParents(selected: Seq[GeneticTree]): Parents = {
    val parentOne = selected.head
    val parentTwo = selected.last

    val targetNodePositionParentOne = selectRandomNodePositionFromTree(parentOne)
    val targetNodeParentOne = parentOne.branches(targetNodePositionParentOne.index)
    val targetNodePositionParentTwo = selectRandomNodePositionFromTree(parentTwo)
    val targetNodeParentTwo = parentTwo.branches(targetNodePositionParentTwo.index)

    val father = new GeneticMaterial(parentOne, targetNodeParentOne, targetNodePositionParentOne)
    val mother = new GeneticMaterial(parentTwo, targetNodeParentTwo, targetNodePositionParentTwo)
    new Parents(mother, father)
  }

  def selectRandomNodePositionFromTree(tree: GeneticTree): Position = {
    val options = tree.branches.indices.toList
    val index = Random.shuffle(options).head
    val position = new Position(1, index)
    position
  }

  def createTreeFromProvidedMaterials(parentMaterial: GeneticMaterial, nodeToInject: MatchNode) = {
    val root = (new SequenceNodeFactory).create()
    val parentNodes = parentMaterial.self.branches
    val generationNr = parentMaterial.self.generation
    val targetIndex = parentMaterial.targetNodePosition.index
    root.children = parentNodes.take(targetIndex - 1) ++ Seq(nodeToInject) ++ parentNodes.drop(targetIndex + 1)
    new GeneticTree(root, generationNr + 1)
  }

}
