import Chat.{Parser, Tokenizer}
import Utils.ClinksCalculator._

/**
  * Mofified by: Alexandra Korukova, Max Caduff
  */
import scala.io.StdIn

object Main extends App {

  // Initialize the menu
  Menu

  println("Bienvenue au Chill-Out !")

  while (true) {
    StdIn.readLine.toLowerCase match {
      case "adieu" | "adieu." => println("À la revoyure !"); System.exit(0)
      case "santé !" => {
        for (i <- 2 to 6) {
          println(s"Nombre de *clinks* pour un santé de $i personnes : ${calculateCombination(i, 2)}.")
        }
      }
      case s => {
        val tokenizer = new Tokenizer(s)
        tokenizer.tokenize()

        val parser = new Parser(tokenizer)
        val expr = parser.parsePhrases()
        val printResult = expr.reply

        println(printResult)
      }
    }
  }
}
