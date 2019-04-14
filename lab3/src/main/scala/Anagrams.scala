import scala.collection.immutable._
import scala.io.Source


/**
  * Modified by: Alexandra Korukova, Max Caduff
  */
object Anagrams extends App {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** A fingerprint is a string which represents a sorted sequence of characters:
   *  Examples:
   *
   *    "aaccx"
   *    "abyz"
   *    "ppp"
   *    ""
   */

  type FingerPrint = String


  /** The dictionary is simply a sequence of words.
   *  You can begin your development with this simple example.
   *  A dictionary of English words is given to you as an external file (linuxwords.txt)
   *  that you can load to use with your program
   */

//  val dictionary: List[Word] =
//    List("ate", "eat", "tea", "pot", "top", "sonja", "jason", "normal",
//         "I", "love", "you", "olive")

  val dictionary: List[Word] = Source.fromFile("linuxwords.txt").getLines().toList

  /** Converts a word/sentence into its fingerprint.
   *  The fingerprint has the same characters as the word, with the same
   *  number of occurrences, but the characters appear in sorted order.
   */

  def fingerPrint(s: Word): FingerPrint = s.toLowerCase sorted

  def fingerPrint(s: Sentence): FingerPrint = fingerPrint(s.reduce(_+_))


  /** `matchingWords` is a `Map` from fingerprints to a sequence of all
   *  the words that have that fingerprint.
   *  This map serves as an easy way to obtain all the anagrams of a word given its fingerprint.
   *
   *  For example, the word "eat" has the fingerprint "aet".
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `matchingWords` map will contain an entry:
   *
   *   "aet"-> List("ate", "eat", "tea")
   */

  val matchingWords: Map[FingerPrint, List[Word]] =
    (for (w <- dictionary) yield fingerPrint(w) -> w). // List((aet, ate), (aet, eat), (aet, tea))
      groupBy(_._1). // Map(aet -> List((aet, ate), (aet, eat), (aet, tea)))
      map(kvlist => (kvlist._1, kvlist._2.map(v => v._2))) // Map(aet -> List(ate, eat, tea))

  println("...testing matchingWords...")
  println(matchingWords("aet"))
  println(matchingWords(fingerPrint("olive")))
  println(matchingWords(fingerPrint(List("at", "e"))))


  /** Returns all the anagrams of a given word. */
  /**
    * Note: this version does not verify if the word has any sense, it just finds all the permutations of the
    * letters in a given word
    */
  def wordAnagramsWithoutDict(word: Word): List[Word] = word match {
    case x if x isEmpty => List("")
    case _ =>
      (for{
        p <- wordAnagramsWithoutDict(word.tail)
        k <- (0 to word.tail.length).toList // the head will be put to the kth position
      } yield (p take k) + word.head + (p drop k)).distinct
  }

  /** Returns all the anagrams of a given word. */
  /**
    * Note: Retrieves all the anagrams of the given word from the dictionary
    */
  def wordAnagrams(word: Word): List[Word] = matchingWords.getOrElse(fingerPrint(word), List())


  // Test code with for example:
   println("...testing wordAnagrams...")
   println(wordAnagrams("eta"))
   println(wordAnagrams("jbdikb"))


  /** Returns the list of all subsequences of a fingerprint.
   *  This includes the fingerprint itself, i.e.
   *  "ko" is a subsequence of "kkoo". It also always includes
   *  the empty string "".
   *
   *  Example: the subsequences of the fingerprint "abbc" are
   *
   *    List("", "c", "b", "bc", "bb", "bbc", "a", "ac", "ab", "abc", "abb", "abbc")
   *
   *  Note that the order of the subsequences does not matter -- the subsequences
   *  in the example above could have been displayed in some other order.
   */
  def subseqs(fp: FingerPrint): List[FingerPrint] =
    for {
      l <- (0 to fp.length).toList // length of the subsequence
      comb <- fp.combinations(l) // cheat
    } yield comb


   // Test code with for example:
   println("...testing subseqs...")
   println(subseqs("abbc"))
   println(subseqs("aabbc"))


  /** Subtracts fingerprint `y` from fingerprint `x`.
   *
   *  The precondition is that the fingerprint `y` is a subsequence of
   *  the fingerprint `x` -- any character appearing in `y` must
   *  appear in `x`.
   */

  def subtract(x: FingerPrint, y: FingerPrint): FingerPrint = (x,y) match {
    case (a, b) if b isEmpty => a // we ran through all the chars of y
    case (c, d) if c isEmpty => "" // should not be the case if y is a subsequence of x
    case _ => {
      if(x.head == y.head) // current char appears in both x and y => subtract it
        subtract(x.tail, y.tail)
      else // current char does not appear in x => leave it
        x.head + subtract(x.tail, y)
    }
  }

  // Test code with for example:
  println("...testing substract...")
  println(subtract("aabbcc", "abc"))


  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the fingerprint of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive","you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    // Find the fingerprint of the sentence
    val fp: FingerPrint = fingerPrint(sentence)

    def loop(fp: FingerPrint): List[Sentence] = {
      if(fp.isEmpty)
        List(List())
      else
        for {
          subseq <- subseqs(fp) // 1. get all possible subsequences of the fingerprint
          matchingWord <- wordAnagrams(subseq) // 2. find the words that have the current subsequence as the fingerprint
          rest <- loop(subtract(fp, subseq)) // 3. recursively do steps 1, 2 with the rest from the current subsequence
        } yield matchingWord :: rest
    }
    loop(fp)
  }

  // Test code with for example:
  println("...testing sentenceAnagrams...")
  println(sentenceAnagrams(List("eat", "tea")))
  println(sentenceAnagrams(List("you", "olive")))
  println(sentenceAnagrams(List("I", "love", "you")))
}