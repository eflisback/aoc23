def getCurveIntersectionPointsX(duration: Long, highScore: Long): Vector[Long] = 
    var pointsX: Vector[Long] = Vector.empty
    val initPrecision = 100
    var precision = initPrecision
    var buttonHoldTime: Long = 0

    var previouslyAboveHighscore = false
    var passedFirstPoint = false
    while buttonHoldTime < duration do
        val traveledDistance = (duration - buttonHoldTime) * buttonHoldTime
        var aboveHighscore = traveledDistance > highScore
        // println(s"Travel distance is $traveledDistance.")
        if aboveHighscore != previouslyAboveHighscore then
            precision = -1 * (precision - 1)
            // println(s"We've passed it. Changing precision to $precision...") 

        if traveledDistance == highScore then 
            println(s"Found point at $traveledDistance")
            pointsX = pointsX :+ buttonHoldTime
            precision = initPrecision
            aboveHighscore = false
            previouslyAboveHighscore = false

        previouslyAboveHighscore = aboveHighscore
        buttonHoldTime += precision
    pointsX

@main def run = 
    val lines = scala.io.Source.fromFile("./data.txt").getLines.toVector
    val duration = lines(0).drop(5).filterNot(_.isWhitespace).toLong
    val highscore = lines(1).drop(9).filterNot(_.isWhitespace).toLong
    val race = (duration, highscore)
    val intersectionPointsX = getCurveIntersectionPointsX(duration, highscore)
    println(intersectionPointsX(1) - intersectionPointsX(0))
