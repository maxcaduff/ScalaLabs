package Chat

import Chat.Tokens._
import Data.{Brand, Product, Products}
import Tree._

// TODO - step 4
/**
  * Mofified by: Alexandra Korukova, Max Caduff
  */
class Parser(tokenizer: Tokenizer) {
  import tokenizer._

  var curTuple: (String, Token) = ("unknown", UNKNOWN)
  
  def curValue: String = curTuple._1
  def curToken: Token = curTuple._2

  /** Reads the next token and assigns it into the global variable curTuple */
  def readToken(): Unit = curTuple = nextToken()

  /** "Eats" the expected token, or terminates with an error. */
  private def eat(token: Token): Boolean = if (token == curToken) {readToken(); true } else false

  /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenClass */
  private def expected(token: Token, more: Token*): ExprTree = {
    BadSyntax("je n'ai pas compris... j'attendais: '" +
      (getName(token) :: more.map(t => getName(t)).toList).mkString("' ou '") +
      "', j'ai trouvé: " + getName(curToken))
  }

  def fatalError(msg: String): Unit = {
    println("Fatal error", msg)
    new Exception().printStackTrace()
//    sys.exit(1)
  }

  /** the root method of the parser: parses an entry phrase */
  def parsePhrases() : ExprTree = {
    eat(BONJOUR)
    if (eat(JE) ) {
      if (eat(ETRE)) {
        if (eat(ASSOIFFE))
          Thirsty()
         // [bonjour] je etre assoiffe
        else if (eat(AFFAME))
          Hungry()
         // [bonjour] je etre affame
        else if (curToken == PSEUDO) {
          val username = curValue.substring(1).toLowerCase()
          readToken()
          Authentication(username)
        } // [bonjour] je suis _pseudo
        else
          expected(ASSOIFFE, AFFAME, PSEUDO)
      } // [bonjour] je etre
      else if (eat(ME)) {
        if (eat(APPELLER)) {
          if (curToken == PSEUDO) {
            val username = curValue.substring(1).toLowerCase()
            readToken()
            Authentication(username)
          }
          else expected(PSEUDO)
        }
        else expected(APPELLER)
      } // [bonjour] je me appeller _pseudo
      else if (eat(VOULOIR)) {
        if(eat(COMMANDER))
          Command(parseOrder())
         // [bonjour] je vouloir commander **order**
        else if (eat(CONNAITRE)) {
          if (eat(MON) && eat(SOLDE))
            Balance()
          else expected(MON, SOLDE)
        } // [bonjour] je vouloir connaitre mon solde
        else expected(COMMANDER, CONNAITRE)
      } // [bonjour] je vouloir
      else expected(ME, ETRE, VOULOIR)
    } // [bonjour] je
    else if (eat(COMBIEN)) {
      if (eat(COUTER))
        Price(parseOrder())
      else expected(COUTER)
    } // [bonjour] combien coute **order**
    else if (eat(QUEL)) {
      if (eat(ETRE) && eat(LE) && eat(PRIX) && eat(DE))
        Price(parseOrder())
      else expected( LE, PRIX, DE)

    }// [bonjour] quel est le prix de **order**
    else expected(BONJOUR, JE, COMBIEN, QUEL)
  }


  /**
    * Parses the order. The order can be the single product (number of products, product name and brand) or the
    * sequence of products with and/or (ET/OU) between them.
    * Note: and/or operator has no priorities one over another, the products are treated in the order they appear
    * in the sentence.
    *
    * For instance, if the user enters "1 bière tenebreuse et 1 croissant ou 1 bière farmer", the resulting ExprTree
    * will look like:
    *
    * And({1 biere tenebreuse}, Or(1 croissant, 1 biere farmer))
    *
    * So, if we consider that the tenebreuse costs 4 CHF, a croissant - 2 CHF and the farmer beer - 1 CHF,
    * the resulting price will be calculated like this:
    *
    * 1. Or(1 croissant, 1 farmer) -> min(2, 1) -> 1
    * 2. And(tenebreuse, 1) -> 4 + 1 -> 5 CHF
    *
    * @return an ExprTree containing the order
    */
  def parseOrder(): ExprTree = {
    // Read the product which probably will become the left operand of And/Or ExprTree
    val left: ExprTree = parseProduct()
    // If ET or OU token is detected, make a recursive call to read And/Or ExprTree right operand
    curToken match {
      case ET =>
        readToken()
        And(left, parseOrder())

      case OU =>
        readToken()
         Or(left, parseOrder())

      case _ => left
    }
  }

  /**
    * Parses the number of products, the product name and its brand (optional)
    * @return the ExprTree containing the number of products, name of the product and the brand of the product
    *         (Items case class)
    *         If the brand is not specified, the default product's brand is returned
    */
  def parseProduct(): ExprTree = {
    var num: Int = 0 // number of products
    var product: Product = null // the ordered product
    var brand: Brand = null // the ordered brand
    // Read number of products
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
