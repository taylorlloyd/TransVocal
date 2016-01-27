
object Driver extends App {
  override def main(args: Array[String]) = {
    if(args.length < 1) {
      println("USAGE: cmd FILENAME")
      System.exit(1)
    }
    val lines = scala.io.Source.fromFile(args(0)).mkString
    Tokenizer.tokenize(lines).foreach( sent => {
      sent.foreach(tok => {
        print(tok+" ")
      })
      println()
    })
  }
}
