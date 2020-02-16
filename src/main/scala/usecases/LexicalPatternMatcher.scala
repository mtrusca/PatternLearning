package usecases

import edu.arizona.sista.processors.{Sentence, Processor}
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import utilities.{DataCleaner}

class LexicalPatternMatcher {

  private val parser: Processor = new CoreNLPProcessor(withDiscourse = false)
  private val cleaner = new DataCleaner

  // http://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html
  private val TAGS_TO_INJECT = List("CC", "CD", "DT", "EX", "FW", "IN", "JJ", "JJR", "JJS", "LS", "MD", "NN",
    "NNS", "NNP", "NNPS", "PDT", "POS", "PRP", "PRP$", "RB", "RBR", "RBS", "RP", "SYM", "TO", "UH", "VB",
    "VBD", "VBG", "VBN", "VBP", "VBZ", "WDT", "WP", "WP$", "WRB"
  )

  def patternMatchesSentence(pattern: List[String], rawSentence: String): Boolean = {
    val sentence = parseSentence(rawSentence)
    if(patternSatisfiesBasicValidations(pattern, sentence)) {
      if(patternLiteralFragmentExists(pattern, sentence)){
        val patternRegex = pattern.mkString(" ").r
        val sentenceStringToMatch = getSentenceStringToMatch(pattern, sentence)
        patternRegex.findFirstIn(sentenceStringToMatch).isDefined
      } else
          false
    } else
        false
  }


  def parseSentence(rawSentence: String): Sentence = {
    val sentence = cleaner.cleanSentence(rawSentence)
    val doc = parser.mkDocument(sentence)
    parser.tagPartsOfSpeech(doc)
    doc.clear()

    if(doc.sentences.isEmpty){
      throw new IllegalStateException("Parsing failed! There are sentences that unparseable")
    }
    val parsedSentence = doc.sentences.head
    val parseIsCorrect = parsedSentence.tags.get.length == parsedSentence.words.length
    if(!parseIsCorrect)
      throw new IllegalStateException("Parsing failed! Some token has no assigned Part-of-Speech tag.")
    else
      parsedSentence
  }


  def patternSatisfiesBasicValidations(pattern: List[String], sentence: Sentence): Boolean = {
    val patternFitsInSentence = sentence.words.length >= pattern.length
    if(patternFitsInSentence){
      val wordsOfSentence = sentence.words
      val biGramsOfSentence = nGrams(2, wordsOfSentence)
      val tagsOfSentence = sentence.tags.get
      val eachTokenInPatternExistsInSentence = pattern.forall(subPattern => {
        wordsOfSentence.exists(_.matches(subPattern)) ||
        tagsOfSentence.exists(_.matches(subPattern)) ||
        biGramsOfSentence.exists(_.matches(subPattern))
      })
      eachTokenInPatternExistsInSentence
    }
    else
      false
  }


  def nGrams(n: Int, words: Array[String]) = words.sliding(n).map( p => p.mkString(" ")).toArray


  def getSentenceStringToMatch(pattern: List[String], sentence: Sentence): String = {

    val patternRequiresTagInjection = pattern.exists(TAGS_TO_INJECT.contains(_))

    if(patternRequiresTagInjection){
      val injectTagsBehindWords = TAGS_TO_INJECT.contains(pattern.last)
      val numberOfTokensLeftSide = getMergePointBetweenWordsAndTags(sentence, pattern, injectTagsBehindWords)

      val leftSide = if(injectTagsBehindWords) sentence.words else sentence.tags.get
      val rightSide = if(injectTagsBehindWords) sentence.tags.get else sentence.words

      val leftTokens = leftSide.indices.filter(_ < numberOfTokensLeftSide).map(leftSide(_))
      val rightTokens = rightSide.indices.filter(_ >= numberOfTokensLeftSide).map(rightSide(_))

      (leftTokens ++ rightTokens).mkString(" ")
    } else {
      sentence.words.mkString(" ")
    }
  }


  def patternLiteralFragmentExists(pattern: List[String], sentence: Sentence): Boolean = {
    val fullSentenceInWords = sentence.words.mkString(" ")
    val patternLiterals = pattern.filter(!TAGS_TO_INJECT.contains(_))
    patternLiterals.mkString(" ")
      .r.findFirstIn(fullSentenceInWords)
      .isDefined
  }


  def getMergePointBetweenWordsAndTags(sentence: Sentence, pattern: List[String], injectTagsBehindWords: Boolean): Int = {

    val fullSentenceInWords = sentence.words.mkString(" ")

    val patternLiterals = pattern.filter(!TAGS_TO_INJECT.contains(_))

    val matchingPatternLiterals = patternLiterals.mkString(" ").r.findFirstIn(fullSentenceInWords)
    val literalStart = fullSentenceInWords.indexOf(matchingPatternLiterals.get)
    val literalEnd = literalStart + matchingPatternLiterals.get.length

    if(injectTagsBehindWords){
      val left = fullSentenceInWords.substring(0, literalEnd)
      val numberOfTokensLeftSide = left.split(" ").length
      numberOfTokensLeftSide
    } else {
      val right = fullSentenceInWords.substring(literalStart, fullSentenceInWords.size - 1)
      val numberOfTokensRightSide = right.split(" ").length
      val numberOfTokensLeftSide = sentence.words.length - numberOfTokensRightSide
      numberOfTokensLeftSide
    }


  }


  def patternHasSingleSplitPoint(pattern: List[String]): Boolean = {
    val patternHeadTypeIsTag = TAGS_TO_INJECT.contains(pattern.head)

    val expectedCountOfHeadType = if(patternHeadTypeIsTag)
      pattern.count(TAGS_TO_INJECT.contains(_))
    else
      pattern.count(!TAGS_TO_INJECT.contains(_))

    val sampleOfExpectedType = pattern.take(expectedCountOfHeadType)
    val sampleIsPure = sampleOfExpectedType.forall(TAGS_TO_INJECT.contains(_))
    sampleIsPure
  }

}

