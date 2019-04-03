package Chat

import Chat.Tokens._
import Data.{Brand, Product, Products}
import Tree._

// TODO - step 4
class Parser(tokenizer: Tokenizer) {
  import tokenizer._

  var curTuple: (String, Token) = ("unknown", UNKNOWN)
  
  def curValue: String = curTuple._1
  def curToken: Token = curTuple._2

  /** Reads the next token and assigns it into the global variable curTuple */
  def readToken(): Unit = curTuple = nextToken()

  /** "Eats" the expected token, or terminates with an error. */
  private def eat(token: Token): Unit = if (token == curToken) readToken() else expected(token)

  /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenClass */
  // TODO (BONUS): find a way to display the string value of the tokens (e.g. "BIERE") instead of their integer value (e.g. 6).
  private def expected(token: Token, more: Token*): Nothing = {
    fatalError(" expected: " +
      (token :: more.toList).mkString(" or ") +
      ", found: " + curToken)
  }

  def fatalError(msg: String): Nothing = {
    println("Fatal error", msg)
    new Exception().printStackTrace()
    sys.exit(1)
  }

  /** the root method of the parser: parses an entry phrase */
  def parsePhrases() : ExprTree = {
    if (curToken == BONJOUR) eat(BONJOUR)
    if (curToken == JE ) {
      eat(JE)
      if (curToken == ETRE) {
        eat(ETRE)
        if (curToken == ASSOIFFE) {
          // Here we do not "eat" the token, because we want to have a custom 2-parameters "expected" if the user gave a wrong token.
          readToken()
          Thirsty()
        } // [bonjour] je etre assoife
        else if (curToken == AFFAME) {
          readToken()
          Hungry()
        } // [bonjour] je etre affame
        else if (curToken == PSEUDO) {
          val username = curValue.substring(1).toLowerCase()
          readToken()
          Authentication(username)
        } // [bonjour] je suis _pseudo
        else expected(ASSOIFFE, AFFAME, PSEUDO)
      } // [bonjour] je etre
      else if (curToken == ME) {
        eat(ME)
        eat(APPELLER)
        val username = curValue.substring(1).toLowerCase()
        Authentication(username)
      } // [bonjour] je me appeller _pseudo
      else if (curToken == VOULOIR) {
        readToken()
        if(curToken == COMMANDER) {
          readToken()
          Command(parseOrder())
        } // [bonjour] je vouloir commander **order**
        else if (curToken == CONNAITRE) {
          readToken()
          eat(MON)
          eat(SOLDE)
          Balance()
        } // [bonjour] je vouloir connaitre mon solde
        else expected(COMMANDER, CONNAITRE)
      } // [bonjour] je vouloir
      else expected(ME, VOULOIR)
    } // [bonjour] je
    else if (curToken == COMBIEN) {
      eat(COMBIEN)
      eat(COUTER)
      Price(parseOrder())
    } // [bonjour] combien coute **order**
    else if (curToken == QUEL) {
      readToken()
      eat(ETRE)
      eat(LE)
      eat(PRIX)
      eat(DE)
      Price(parseOrder())
    }// [bonjour] quel est le prix de **order**
    else expected(BONJOUR, JE, COMBIEN, QUEL)

  }


  def parseOrder(): ExprTree = {
    val left: ExprTree = parseProduct()
    curToken match {
      case ET => {
        readToken()
        And(left, parseOrder())
      }
      case OU => {
        readToken()
         Or(left, parseOrder())
      }
      case _ => left
    }
  }

  /**
    * Reads the order containing N products of the same type
    * The order is composed of the number of products, name of the product and the brand of the product (optional)
    * @return the Tuple3 containing the number of products (._1), name of the product (._2) and the brand of the product
    *         If the brand is not specified, the default product's brand is return
    */
  def parseProduct(): ExprTree = {
    var num: Int = 0 // number of products
    var product: Product = null // the ordered product
    var brand: Brand = null // the ordered brand
    // Read number of products (mandatory)
    if (curToken == NUM) {
      num = curValue.toInt
    } else expected(NUM)
    readToken()
    // Read the product name
    if(isProduct(curToken)) {
      product = Products.getProductByName(curValue)
    } else expected(BIERE, CHIPS, CROISSANT)
    readToken()
    // Read the brand if it specified, set the brand to the default product's one otherwise
    brand = product.defaultBrand
    if(isBrand(curToken)) {
      brand = product.getBrandByName(curValue)
      eat(curToken)
    }
    Items(Item(product, brand), num)
  }

  // Start the process by reading the first token.
  readToken()
}
