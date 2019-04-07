package Utils

/**
* Contains the dictionary of the application, which is used to validate, correct and normalize words entered by the
* user.
*
*  Modified by: Alexandra Korukova, Max Caduff
*/
object Dictionary {
  // This dictionary is a Map object that contains valid words as keys and their normalized equivalents as values (e.g.
  // we want to normalize the words "veux" and "aimerais" in on unique term: "vouloir").
  val dictionary: Map[String, String] = Map(
    "bonjour" -> "bonjour",
    "hello" -> "bonjour",
    "yo" -> "bonjour",
    "je" -> "je",
    "j" -> "je",
    "suis" -> "etre",
    "est" -> "etre",
    "veux" -> "vouloir",
    "aimerais" -> "vouloir",
    "voudrais" -> "vouloir",
    "bière" -> "biere",
    "bières" -> "biere",
    "croissant" -> "croissant",
    "croissants" -> "croissant",
    "et" -> "et",
    "ou" -> "ou",
    "assoiffé" -> "assoiffe",
    "assoiffée" -> "assoiffe",
    "affamé" -> "affame",
    "affamée" -> "affame",
    "m" -> "me",
    "appelle" -> "appeller",
    "commander" -> "commander",
    "connaitre" -> "connaitre",
    "connaître" -> "connaitre",
    "savoir" -> "connaitre",
    "maison" -> "maison",
    "cailler" -> "cailler",
    "farmer" -> "farmer",
    "boxer" -> "boxer",
    "wittekop" -> "wittekop",
    "punkipa" -> "punkipa",
    "jackhammer" -> "jackhammer",
    "tenebreuse" -> "tenebreuse",
    "mon" -> "mon",
    "solde" -> "solde",
    "vaut" -> "couter",
    "valent" -> "couter",
    "fait" -> "couter",
    "font" -> "couter",
    "coute" -> "couter",
    "couter" -> "couter",
    "coutent" -> "couter",
    "combien" -> "combien",
    "prix" -> "prix",
    "quel" -> "quel",
    "quels" -> "quel",
    "le" -> "le",
    "la" -> "le",
    "l'" -> "le",
    "de" -> "de",
    "du" -> "de",
    "d'" -> "de",
    "un" -> "1",
    "une" -> "1"
  )
}
