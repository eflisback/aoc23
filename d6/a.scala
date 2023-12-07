def getWinOpportunityHoldDurations(raceDuration: Int, highScore: Int): Vector[Int] = 
    var possibleRangeDurations: Vector[Int] = Vector.empty
    var speed = 0
    for s <- 0 until raceDuration do
        speed = s
        var traveledDistance = 0
        for t <- (raceDuration - s) until 0 by -1 do
            traveledDistance += speed
        if traveledDistance > highScore then 
            possibleRangeDurations = possibleRangeDurations :+ s
    possibleRangeDurations

@main def run = 
    val lines = scala.io.Source.fromFile("./data.txt").getLines.toVector
    val durations = lines(0).drop(5).split(' ').filterNot(_.isEmpty).map(_.toInt)
    val highscore = lines(1).drop(9).split(' ').filterNot(_.isEmpty).map(_.toInt)
    val races = for i <- durations.indices yield (durations(i), highscore(i))
    
    val winOpportunitiesAllRaces = for race <- races yield getWinOpportunityHoldDurations(race._1, race._2)
    println(winOpportunitiesAllRaces.map(_.length).reduce(_ * _))