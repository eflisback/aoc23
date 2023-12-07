def getNumberOfWinOpportunities(duration: Long, highScore: Long): List[Long] =
  var toReturn: List[Long] = List.empty
  var buttonHoldTime: Long = 0
  var previouslyAboveHighscore = false
  while buttonHoldTime <= duration do
    val traveledDistance = (duration - buttonHoldTime) * buttonHoldTime
    val isAboveHighscore = traveledDistance > highScore
    if isAboveHighscore && !previouslyAboveHighscore then
      toReturn = toReturn :+ buttonHoldTime
    else if !isAboveHighscore && previouslyAboveHighscore then
      toReturn = toReturn :+ buttonHoldTime
    previouslyAboveHighscore = isAboveHighscore
    buttonHoldTime += 1
  toReturn

@main def run =
  val lines = scala.io.Source.fromFile("./data.txt").getLines.toVector
  val duration = lines(0).drop(5).filterNot(_.isWhitespace).toLong
  val highscore = lines(1).drop(9).filterNot(_.isWhitespace).toLong
  val race = (duration, highscore)
  val winOpportunityPoints = getNumberOfWinOpportunities(duration, highscore)
  val differences =
    winOpportunityPoints.grouped(2).map(pair => pair(1) - pair(0))
  println(differences.mkString)
