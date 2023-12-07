def getWinOpportunityHoldDurations(raceDuration: Int, highScore: Int): IndexedSeq[Int] =
    for 
        s <- 0 until raceDuration
        traveledDistance = (raceDuration - s) * s
        if traveledDistance > highScore
    yield s

@main def run = 
    val lines = scala.io.Source.fromFile("./data.txt").getLines.toVector
    val durations = lines(0).drop(5).split(' ').filterNot(_.isEmpty).map(_.toInt)
    val highscore = lines(1).drop(9).split(' ').filterNot(_.isEmpty).map(_.toInt)
    val races = for i <- durations.indices yield (durations(i), highscore(i))
    val winOpportunitiesAllRaces = for race <- races yield getWinOpportunityHoldDurations(race._1, race._2)
    println(winOpportunitiesAllRaces.map(_.length).reduce(_ * _))