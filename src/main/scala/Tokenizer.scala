object Tokenizer {
  import opennlp.tools.sentdetect._
  import opennlp.tools.tokenize._

  lazy val sentModel = new SentenceModel(getClass().getResource("en-sent.bin"))
  lazy val sentDetect = new SentenceDetectorME(sentModel)

  lazy val tokModel = new TokenizerModel(getClass().getResource("en-token.bin"))
  lazy val tokDetect = new TokenizerME(tokModel)

  def tokenize(text:String, splitter:String=" ") = {
    sentDetect.sentDetect(text).map(sent => tokDetect.tokenize(sent))
  }
}
