import Chat.{Parser, Tokenizer}
import Utils.ClinksCalculator._
import Data.{Brand, Product, Products}

import scala.io.StdIn

object Main extends App {

//  testProducts()

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

  def testProducts(): Unit = {
    val punk: Brand = new Brand("PunkIPA", 3)
    val boxer: Brand = new Brand("Boxer", 1)

    val biere: Product = new Product("Biere", Set[Brand](punk, boxer), boxer)

    print("Bieres: \n" + biere.toString + "\n")

    val crMaison: Brand = new Brand("Maison", 2)
    val crCailler: Brand = new Brand("Cailler", 2)

    val croissant = new Product("Croissant", Set[Brand](crMaison, crCailler), crMaison)

    val products = Products
    products.addProducts(List(biere, croissant))

    print("\nMenu: \n" + products.toString + "\n")
  }
}
