object Isogram {

  def isIsogram(s: String): Boolean =
    !s.toLowerCase()
      .groupBy(identity)
      .filterKeys(x => !x.isSpaceChar && x != '-')
      .exists(_._2.length > 1)
}
