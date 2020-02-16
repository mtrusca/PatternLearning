package domain

import edu.arizona.sista.processors.{Sentence, Processor}
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import utilities.{DataCleaner}

object Parser {
  private val parser: Processor = new CoreNLPProcessor(withDiscourse = false)
  private val cleaner = new DataCleaner

  def parseReview(reviewBody: Seq[String]): Seq[Sentence] = reviewBody.map(parseSentence)

  def parseSentence(rawSentence: String): Sentence = {
    val sentence = cleaner.cleanSentence(rawSentence)
    val doc = parser.mkDocument(sentence)
    parser.tagPartsOfSpeech(doc)
    doc.clear()

    if(doc.sentences.isEmpty){
      throw new IllegalStateException("Parsing failed! There are sentences that are unparseable")
    }
    val parsedSentence = doc.sentences.head
    val parseIsCorrect = parsedSentence.tags.get.length == parsedSentence.words.length
    if(!parseIsCorrect)
      throw new IllegalStateException("Parsing failed! Some token has no assigned Part-of-Speech tag.")
    else
      parsedSentence
  }

  def getVerbPhraseChunks(sentence: Sentence, withoutStopwords: Boolean = false) = {
    if(withoutStopwords)
      removeStopwords(getChunks(sentence, "B-VP"))
    else
      getChunks(sentence, "B-VP")
  }

  def getNounPhraseChunks(sentence: Sentence, withoutStopwords: Boolean = false): Seq[String] = {
    if(withoutStopwords)
      removeStopwords(getChunks(sentence, "B-NP"))
    else
      getChunks(sentence, "B-NP")
  }

  def getChunks(sentence: Sentence, filter: String = "B"): Seq[String] = {
    val chunkIndicators = sentence.chunks.get
    val words = sentence.words

    val starts = chunkIndicators.indices.filter(c => {
      val value = chunkIndicators(c)
      value.contains("B")
    })

    val targetStarts = chunkIndicators.indices.filter(c => {
      val value = chunkIndicators(c)
      value.contains(filter)
    })

    // We are only interested in the begin and inner parts of a chunk/phrase
    // Not the outside BIO (begin, inside, outside)
    val outsides = chunkIndicators.indices.filter(c => {
      val value = chunkIndicators(c)
      value.equals("O")
    })

    targetStarts.map(s => {
      val following = (starts ++ outsides).filter(_ > s).sorted

      if(following.isEmpty) {
        val wordsRemaining = s < words.length
        if(wordsRemaining)
          words.slice(s, words.length).mkString(" ")
        else
          words(s)
      }
      else
        words.slice(s, following.head).mkString(" ")
    })
  }

  def removeStopwords(chunks: Seq[String]) = {
    chunks.map(removeStopwordsFromChunk).filterNot(_.length == 0)
  }

  def removeStopwordsFromChunk(chunk: String) = {
    chunk.split(" ")
      .filterNot(stopWords.get.contains)
      .mkString(" ")
      .trim
  }

}
