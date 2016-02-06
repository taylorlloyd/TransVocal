import scala.collection.mutable.ListBuffer

object Semantic {
  def analyze(sentence: Sentence) = {
    // Collect the chunks into groups
    val chunks = ListBuffer[Any]()
    var curChunk = ListBuffer[Token]()
    sentence.tokens.foreach( tok => {
      if((tok.chunk.startsWith("B") || tok.chunk.startsWith("O")) && curChunk.size>0) {
        chunks+=analyzeChunk(curChunk)
        curChunk=ListBuffer[Token]()
      }
      curChunk+=tok
    })
    chunks:+analyzeChunk(curChunk)

  }

  private def analyzeChunk(chunk:Seq[Token]): Parse = {
    val richToks = chunk.map(richToken)
    if(chunk(0).chunk == "O")
      richToks(0)
    else
    chunk(0).chunk.substring(2) match {
      case "NP" => NounChunk(richToks)
      case "PP" => PrepositionChunk(richToks)
      case "VP" => VerbChunk(richToks)
      case "ADVP" => AdverbChunk(richToks)
      case "ADJP" => AdjectiveChunk(richToks)
      case "SBAR" => throw new RuntimeException("Unsupported Chunk Type")
      case "PRT" => throw new RuntimeException("Unsupported Chunk Type")
      case "INTJ" => throw new RuntimeException("Unsupported Chunk Type")
    }
  }

  private def richToken(tok: Token) = {
    println(tok)
    tok.pos match {
      case "CC" => throw new RuntimeException("Unsupported token")
      case "CD" => throw new RuntimeException("Unsupported token")
      case "DT" => DeterminerWord(tok.text)
      case "EX" => throw new RuntimeException("Unsupported token")
      case "FW" => throw new RuntimeException("Unsupported token")
      case "IN" => ConjunctionWord(tok.text)
      case "JJ" => AdjectiveWord.parse(tok)
      case "JJR" => AdjectiveWord.parse(tok)
      case "JJS" => AdjectiveWord.parse(tok)
      case "LS" => throw new RuntimeException("Unsupported token")
      case "MD" => throw new RuntimeException("Unsupported token")
      case "NN" => NounWord.parse(tok)
      case "NNS" => NounWord.parse(tok)
      case "NNP" => NounWord.parse(tok)
      case "NNPS" => NounWord.parse(tok)
      case "PDT" => throw new RuntimeException("Unsupported token")
      case "POS" => PosessiveWord(tok.text)
      case "PRP" => NounWord.parse(tok)
      case "PRP$" => NounWord.parse(tok)
      case "RB" => AdverbWord.parse(tok)
      case "RBR" => AdverbWord.parse(tok)
      case "RBS" => AdverbWord.parse(tok)
      case "RP" => AdverbWord.parse(tok)
      case "SYM" => throw new RuntimeException("Unsupported token")
      case "TO" => throw new RuntimeException("Unsupported token")
      case "UH" => throw new RuntimeException("Unsupported token")
      case "VB" => VerbWord.parse(tok)
      case "VBD" => VerbWord.parse(tok)
      case "VBG" => VerbWord.parse(tok)
      case "VBN" => VerbWord.parse(tok)
      case "VBP" => VerbWord.parse(tok)
      case "VBZ" => VerbWord.parse(tok)
      case "WDT" => throw new RuntimeException("Unsupported token")
      case "WP" => throw new RuntimeException("Unsupported token")
      case "WP$" => throw new RuntimeException("Unsupported token")
      case "WRB" => throw new RuntimeException("Unsupported token")
      case "." => SentenceEnd(tok.text)
      case "," => Comma(tok.text)
      case ":" => Colon(tok.text)
      case "(" => OpenParen(tok.text)
      case ")" => CloseParen(tok.text)
      case _ => throw new RuntimeException("Unrecognized token")
    }
  }
  trait Parse {
    /**
     * Print out this tree as it would have been read in
     */
    def toPlainString: String
  }

  trait Word extends Parse {
    def word: String
    def toPlainString = word
  }

  case class SentenceEnd(val word: String) extends Word
  case class Comma(val word: String) extends Word
  case class Colon(val word: String) extends Word
  case class OpenParen(val word: String) extends Word
  case class CloseParen(val word: String) extends Word

  trait Chunk extends Parse {
    def words: Seq[Word]
    def toPlainString = {
      if(words.size == 0) ""
      else {
        val sb = new StringBuilder(words(0).word)
        words.drop(1).foreach(w => {
          if (w.isInstanceOf[PosessiveWord])
            sb.append(w.word)
          else
            sb.append(" "+w.word)
        })
        sb.toString
      }
    }
  }

  case class AdverbChunk(val words: Seq[Word]) extends Chunk with Adverb {
    private def adverbTok = words.find(_.isInstanceOf[Adverb]).get.asInstanceOf[Adverb]
    def comparative=adverbTok.comparative
    def superlative=adverbTok.superlative
    def particle=adverbTok.particle
  }

  case class AdjectiveChunk(val words: Seq[Word]) extends Chunk with Adjective {
    private def adjectiveTok = words.find(_.isInstanceOf[Adjective]).get.asInstanceOf[Adjective]
    def comparative=adjectiveTok.comparative
    def superlative=adjectiveTok.superlative
  }

  case class NounChunk(val words: Seq[Word]) extends Chunk with Noun {
    private def nounTok = words.find(w =>
      w.isInstanceOf[Noun] && !w.asInstanceOf[Noun].posessive).get.asInstanceOf[Noun]
    def plural = nounTok.plural
    def proper = nounTok.proper
    def pronoun = nounTok.pronoun
    def posessive = false
  }

  case class VerbChunk(val words: Seq[Word]) extends Chunk with Verb {
    private def verbTok = words.find(_.isInstanceOf[Verb]).get.asInstanceOf[Verb]
    def tense = verbTok.tense
  }

  case class PrepositionChunk(val words: Seq[Word]) extends Chunk {
  }

  trait Adverb {
    def comparative:Boolean
    def superlative:Boolean
    def particle:Boolean
  }

  trait Adjective {
    def comparative:Boolean
    def superlative:Boolean
  }

  trait Noun {
    def plural:Boolean
    def proper:Boolean
    def pronoun:Boolean
    def posessive:Boolean
  }

  trait Verb {
    def tense:String
  }

  object Verb {
    val BASE="Base"
    val THIRDPRESENT="3rd Person Singular Present"
    val PRESENT="1st/2nd Singular Present"
    val PAST="Past Tense"
    val PASTPART="Past Participle"
    val PRESENTPART="Present Participle"
  }

  case class AdverbWord(val word:String, val comparative:Boolean, val superlative:Boolean, val particle:Boolean) extends Adverb with Word
  object AdverbWord {
    def parse(tok:Token) = {
      tok.pos match {
        case "RB" => AdverbWord(tok.text, false, false, false)
        case "RBR" => AdverbWord(tok.text, true, false, false)
        case "RBS" => AdverbWord(tok.text, false, true, false)
        case "RP" => AdverbWord(tok.text, false, false, true)
      }
    }
  }

  case class AdjectiveWord(val word:String, val comparative:Boolean, val superlative:Boolean) extends Adjective with Word
  object AdjectiveWord {
    def parse(tok:Token) = {
      tok.pos match {
        case "JJ" => AdjectiveWord(tok.text, false, false)
        case "JJR" => AdjectiveWord(tok.text, true, false)
        case "JJS" => AdjectiveWord(tok.text, false, true)
      }
    }
  }

  case class NounWord(val word:String, val plural:Boolean, val proper:Boolean) extends Noun with Word {
    def pronoun = false
    def posessive = false
  }

  case class PronounWord(val word:String, val plural:Boolean, val posessive:Boolean) extends Noun with Word {
    def pronoun = true
    def proper = false
  }

  case class PosessiveWord(val word:String) extends Word

  case class ConjunctionWord(val word:String) extends Word

  case class DeterminerWord(val word:String) extends Word

  object NounWord {
    def parse(tok:Token) = {
      tok.pos match {
        case "NN" => NounWord(tok.text, false, false)
        case "NNS" => NounWord(tok.text, true, false)
        case "NNP" => NounWord(tok.text, false, true)
        case "NNPS" => NounWord(tok.text, true, true)
        case "PRP" => PronounWord(tok.text, false, false)
        case "PRP$" => PronounWord(tok.text, false, true)
      }
    }
  }

  case class VerbWord(val word:String, val tense:String) extends Verb with Word
  object VerbWord {
    def parse(tok:Token) = {
      tok.pos match {
        case "VB" => VerbWord(tok.text, Verb.BASE)
        case "VBZ" => VerbWord(tok.text, Verb.THIRDPRESENT)
        case "VBP" => VerbWord(tok.text, Verb.PRESENT)
        case "VBD" => VerbWord(tok.text, Verb.PAST)
        case "VBN" => VerbWord(tok.text, Verb.PASTPART)
        case "VBG" => VerbWord(tok.text, Verb.PRESENTPART)
      }
    }
  }

}
