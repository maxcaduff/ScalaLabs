package Chat

/**
  * Modified by: Alexandra Korukova, Max Caduff
  */
object Tokens {
  type Token = Int

  // Terms
  val BONJOUR: Token     = 0
  val JE: Token          = 1
  val ME: Token          = 35
  // Actions
  val ETRE: Token        = 2
  val VOULOIR: Token     = 3
  val APPELLER: Token    = 34
  // Operators
  val ET: Token          = 4
  val OU: Token          = 5
  // Products
  val BIERE: Token       = 6
  val CROISSANT: Token   = 7
  val CHIPS: Token       = 15
  // Brands
  val MAISON: Token      = 16
  val CAILLER: Token     = 17
  val FARMER: Token      = 18
  val BOXER: Token       = 19
  val WITTEKOP: Token    = 20
  val PUNKIPA: Token     = 21
  val JACKHAMMER: Token  = 22
  val TENEBREUSE: Token  = 23
  // Utils
  val PSEUDO: Token      = 9
  val NUM: Token         = 10
  val COMMANDER: Token   = 24
  val CONNAITRE: Token   = 25
  val MON: Token         = 26
  val SOLDE: Token       = 27
  val COMBIEN: Token     = 28
  val COUTER: Token      = 29
  val QUEL: Token        = 30
  val LE: Token          = 31
  val PRIX: Token        = 32
  val DE: Token          = 33

  val UNKNOWN: Token     = 11
  val EOL: Token         = 12
  // Test
  val ASSOIFFE : Token   = 13
  val AFFAME : Token     = 14

  /**
    * Indicates if the provided token corresponds to a product
    * @param t the token in question
    * @return true if the token corresponds to a product
    */
  def isProduct(t: Token): Boolean = {
    t == BIERE || t == CROISSANT || t == CHIPS
  }

  /**
    * Indicates if the provided token corresponds to a brand
    * @param t the token in question
    * @return true if the token corresponds to a brand
    */
  def isBrand(t: Token): Boolean = {
     t == MAISON ||
     t == CAILLER ||
     t == FARMER ||
     t == BOXER ||
     t == WITTEKOP ||
     t == PUNKIPA ||
     t == JACKHAMMER ||
     t == TENEBREUSE
  }

  def getName (token: Token): String = {
    token match {
      case BONJOUR => "bonjour"
      case JE => "je"
      case ETRE => "etre"
      case VOULOIR => "vouloir"
      case BIERE => "biere"
      case CROISSANT => "croissant"
      case ET => "et"
      case OU => "ou"
      case ASSOIFFE => "assoiffe"
      case AFFAME => "affame"
      case ME => "me"
      case APPELLER => "appeller"
      case COMMANDER => "commander"
      case CONNAITRE => "connaitre"
      case MAISON => "maison"
      case CAILLER => "cailler"
      case FARMER => "farmer"
      case BOXER => "boxer"
      case WITTEKOP => "wittekop"
      case PUNKIPA => "punkipa"
      case JACKHAMMER => "jackhammer"
      case TENEBREUSE => "tenebreuse"
      case MON => "mon"
      case SOLDE => "solde"
      case COUTER => "couter"
      case COMBIEN => "combien"
      case PRIX => "prix"
      case QUEL => "quel"
      case LE => "le"
      case DE => "de"
      case UNKNOWN => "unknown"
      case EOL => "end of line"
      case NUM => "number"
      case PSEUDO => "pseudo"
      case _ => "undefined token"
    }
  }


}
