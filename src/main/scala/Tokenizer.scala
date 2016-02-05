case class Token(val text:String, val pos:String, val chunk:String)
case class Sentence(val Tokens: Seq[Token])
object Tokenizer {
  import opennlp.tools.sentdetect._
  import opennlp.tools.tokenize._
  import opennlp.tools.postag._
  import opennlp.tools.chunker._

  lazy val sentModel = new SentenceModel(getClass().getResource("en-sent.bin"))
  lazy val sentDetect = new SentenceDetectorME(sentModel)

  lazy val tokModel = new TokenizerModel(getClass().getResource("en-token.bin"))
  lazy val tokDetect = new TokenizerME(tokModel)

  lazy val posModel = new POSModel(getClass().getResource("en-pos-maxent.bin"))
  lazy val posDetect = new POSTaggerME(posModel)

  lazy val chunkModel = new ChunkerModel(getClass().getResource("en-chunker.bin"))
  lazy val chunkDetect = new ChunkerME(chunkModel)

  def tokenize(text:String, splitter:String=" "): Seq[Sentence] =
    sentDetect.sentDetect(text).map(sent => {
      val tokens = tokDetect.tokenize(sent)
      val pos = posDetect.tag(tokens)
      val chunks = chunkDetect.chunk(tokens, pos)
      val tups = (tokens,pos,chunks).zipped.toList
      Sentence(tups.map(t => Token(t._1,t._2,t._3)))
    })
}
