type RangePair = ((Long, Long), (Long, Long))

extension (thisRange: (Long, Long))
  def contains(n: Long): Boolean = (thisRange._1 <= n) && (n < thisRange._2)
  def contains(other: (Long, Long)): Boolean = (thisRange._1 <= other._1) && (thisRange._2 < other._2)

def getRangePair(line: String): RangePair =
  val parts = line.split(' ').map(_.toLong)
  ((parts(0), parts(0) + parts(2)), (parts(1), parts(1) + parts(2)))

@main def run =
  val lines = scala.io.Source.fromFile("./data.txt").getLines.toVector
  var categories: Vector[Vector[RangePair]] = Vector.empty
  var categoryIndex = 0

  def addToCategories(rp: RangePair): Unit =
    if categoryIndex >= categories.length then
      categories = categories :+ Vector.empty
    categories =
      categories.updated(categoryIndex, categories(categoryIndex) :+ rp)

  for line <- lines.drop(3) do
    if line.isBlank then categoryIndex += 1
    else if !line(0).isDigit then {} else addToCategories(getRangePair(line))

  val initialSeedRanges: Array[(Long, Long)] =
    val rangeLimits = lines(0).drop(7).split(' ').map(_.toLong)
    (for i <- 0 until rangeLimits.length / 2
    yield (rangeLimits(i * 2), rangeLimits(i * 2) + rangeLimits(i * 2 + 1))
    ).toArray

  def processRangesThroughCategory(rangePair: RangePair, inputRanges: Vector[(Long, Long)]): Vector[(Long, Long)] =
    inputRanges.flatMap {
      case (inputStart, inputEnd) =>
        val (sourceStart, sourceEnd) = rangePair._2 // Use the second range in RangePair as the source range
        if (sourceStart <= inputStart) && (inputEnd <= sourceEnd) then
          // Calculate offset and apply it to input range
          val offset = inputStart - sourceStart
          val outputStart = rangePair._1._1 + offset
          val outputEnd = outputStart + (inputEnd - inputStart)
          Some((outputStart, outputEnd))
        else
          // If input range can't be processed, split it into smaller ranges
          val intersectionStart = Math.max(inputStart, sourceStart)
          val intersectionEnd = Math.min(inputEnd, sourceEnd)
          if intersectionStart < intersectionEnd then
            // Non-empty intersection, split the range
            val remainingRanges = Vector(
              (inputStart, intersectionStart),
              (intersectionEnd, inputEnd)
            )
            remainingRanges.flatMap(remainingRange => processRangesThroughCategory(rangePair, remainingRanges))
          else
            // No intersection, keep the original range
            Some((inputStart, inputEnd))
    }

  var lowest: Long = Long.MaxValue

  initialSeedRanges.foreach { range =>
    println(s"Getting started on range ${range._1} until ${range._2}...")
    var rangeStack: Vector[(Long, Long)] = Vector(range)
    for ((category, i) <- categories.zipWithIndex) do
      rangeStack = category.flatMap(rangePair => processRangesThroughCategory(rangePair, rangeStack))
    val lowestRangeStart = rangeStack.map(_._1).min
    if (lowestRangeStart < lowest) then lowest = lowestRangeStart
  }

  println(lowest)
