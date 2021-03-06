package Chat

import Chat.Tokens._
import Utils.Dictionary.dictionary
import Utils.SpellChecker._

/**
  * Modified by: Alexandra Korukova, Max Caduff
  */
class Tokenizer(input: String) {
  var tokens: Array[(String, Token)] = Array()
  var currentTokenIndex: Token = -1

  private def getTokenFromString(s: String): Token = s match {
    case "bonjour" => BONJOUR
    case "je" => JE
    case "etre" => ETRE
    case "vouloir" => VOULOIR
    case "et" => ET
    case "ou" => OU
    case "biere" => BIERE
    case "croissant" => CROISSANT
    case "assoiffe" => ASSOIFFE
    case "affame" => AFFAME
    case "me" => ME
    case "appeller" => APPELLER
    case "commander" => COMMANDER
    case "connaitre" => CONNAITRE
    case "maison" => MAISON
    case "cailler" => CAILLER
    case "farmer" => FARMER
    case "boxer" => BOXER
    case "wittekop" => WITTEKOP
    case "punkipa" => PUNKIPA
    case "jackhammer" => JACKHAMMER
    case "tenebreuse" => TENEBREUSE
    case "mon" => MON
    case "solde" => SOLDE
    case "couter" => COUTER
    case "combien" => COMBIEN
    case "quel" => QUEL
    case "prix" => PRIX
    case "le" => LE
    case "de" => DE
    case p if p.startsWith("_") && p.length > 1 => PSEUDO // If the word starts with '_' and has more than one character it is a pseudonym.
    case n if n.forall(Character.isDigit) => NUM // If every character is a number, the word thus is a number.
    case _ => UNKNOWN
  }

  def tokenize(): Unit = {
    val words = input
      .trim()
      .replaceAll("[.|,|!|?|*]", " ") // Remove punctuation.
      .replaceAll(" +|[']", " ") // Remove multiple spaces and replace apostrophes by a space.
      .split(" ")

    // Get each word's occurence in the dictionary or check for the closest word if it is not contained in the dictionary.
    val fromDictionnary = words.map(w => dictionary.getOrElse(w, getClosestWordInDictionary(w)))

    tokens = fromDictionnary.map(t => (t, getTokenFromString(t)))
  }

  def nextToken(): (String, Token) = {
    currentTokenIndex += 1

    if (currentTokenIndex < tokens.length) {
      tokens(currentTokenIndex)
    } else {
      ("EOL", EOL)
    }
  }
}
