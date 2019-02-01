object Anagram {

  def findAnagrams(word: String, candidates: List[String]): List[String] =
    candidates
      .filter(isAnagram(_, word))

  private def isAnagram(word: String, candidate: String): Boolean = {
    lazy val normalizedWord = word.toLowerCase()
    lazy val normalizedCandidate = candidate.toLowerCase()
    word.length == candidate.length &&
    normalizedWord != normalizedCandidate &&
    normalizedWord.sorted == normalizedCandidate.sorted
  }

}
