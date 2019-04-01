package Chat

object Tokens {
  type Token = Int

  // Terms
  val BONJOUR: Token     = 0
  val JE: Token          = 1
  // Actions
  val ETRE: Token        = 2
  val VOULOIR: Token     = 3
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
  val COUTE: Token       = 29
  val COUTENT: Token     = 29
  val QUEL: Token        = 30
  val LE: Token          = 31
  val PRIX: Token        = 32
  val DE: Token          = 33

  val UNKNOWN: Token     = 11
  val EOL: Token         = 12
  // Test
  val ASSOIFFE : Token = 13
  val AFFAME : Token = 14
}
