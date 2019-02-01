object Anagram {

  def findAnagrams(word: String, candidates: List[String]): List[String] =
    candidates
      .filter(isAnagram(_, word))

  private def isAnagram(word: String, candidate: String): Boolean = {
    val normalizedWord = word.toLowerCase()
    val normalizedCandidate = candidate.toLowerCase()
    normalizedCandidate.length == normalizedWord.length &&
    normalizedWord != normalizedCandidate &&
    normalizedCandidate.sorted == normalizedWord.sorted
  }

}
