package forcomp

import forcomp.Anagrams.{sentenceAnagrams, wordOccurrences}

import java.util
import scala.io.{Codec, Source}

object Anagrams extends AnagramsInterface:

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences =
    // lowercase the word
    val lowerW = w.toLowerCase()

    // group same characters together in a map of character and a list
    // example : "seed" will become Map[(s, List[s]),(e, List(e,e)), (d, List(d))

    val groupChar = lowerW.groupBy( ch => ch)
    // convert map to list
    // for each character and list tuple, take the list out and count occurrences using foldLeft
    // put back the count as 2nd element of the tuple with character as the first element.
    val occList = groupChar.toList.map( xs => (xs._1, xs._2.foldLeft(0)((x,y) => x + 1)))


    // sort the occurrence list using sortWith function
    occList.sortWith( (x,y) => x._1.compareTo(y._1) < 0)


  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences =
    //combine words in sentence to form single word
    val combine = s.foldLeft[Word]("")(_+_)
    //convert sentence to word
    wordOccurrences(combine)



  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    // group dictionary words and create (key , value) pair by occurrence
    dictionary.groupBy(word => wordOccurrences(word))


  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] =
    //return value from dictionary occurrences
    dictionaryByOccurrences.get(wordOccurrences(word)) match
      case None => List(word)
      case Some(lst) => lst

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] =
    occurrences match
      case Nil => List[Occurrences](List())
      // for head of the occurrences, we have two options 1. keep 1 to max num in set 2. do not keep in set
      // keeping in set : how many to keep in set ?
      case x::xs =>


        val withZero = (for
          i <- (0 to x._2).toList
          remaining <- combinations(xs)
        yield (x._1, i) :: remaining)

        //remove zero : filter out 0
        withZero.map(lst => lst.filter((ch, num) => num > 0))

        //decide on number of characters to be kept for x
        // loop from 0 to all
        /*
        val remaining1: Seq[Occurrences] = combinations(xs)
        val withZero1 = (for
          i <- (0 to x._2).toList
        yield {
            List((x._1, i)) +: remaining1
        })
        //withZero
        */









  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences =
    //take map of x as first parameter in foldleft
    val xMap = Map[Char, Int]() ++ x
    //for each element of y check if in map and update mao accordingly.
    //convert to list
    // sort and return
    y.foldLeft(xMap){ case (xmap, (ch,num)) =>
      if xmap(ch) - num == 0
      then xmap.removed(ch)
      else xmap.updated(ch, xmap(ch) - num)
    }.toList.sortWith((x1, y1) => x1._1.compareTo(y1._1) < 0)  // sort the occurrence list using sortWith function







  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] =

    def helperAnagram(occurrence: Occurrences): List[Sentence] =
      //println(occurrence)
      if(occurrence.isEmpty)
        List[Sentence](List())
      else
          //get the combinations
          //for each combination get the anagrams and recur for remaining characters
          //combinations(occurrence).flatMap(candidate => dictionaryByOccurrences(candidate).flatMap( word1 => helperAnagram(subtract(occurrence,candidate)).map( sen => word1 :: sen)))
          for
            subset <- combinations(occurrence)
            anagram <- dictionaryByOccurrences getOrElse (subset, Nil)
            sentence <- helperAnagram(subtract(occurrence, subset))
            if !subset.nonEmpty
          yield
            sentence.::(anagram)

    helperAnagram(sentenceOccurrences(sentence))



object Dictionary:
  def loadDictionary: List[String] =
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try
      val s = Source.fromInputStream(wordstream)(Codec.UTF8)
      s.getLines().toList
    catch
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    finally
      wordstream.close()