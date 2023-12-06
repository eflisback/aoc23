type RangePair = ((Long, Long), (Long, Long))

extension (thisRange: (Long, Long))
  def contains(n: Long): Boolean = (thisRange._1 <= n) && (n < thisRange._2)
  def contains(other: (Long, Long)): Boolean = (thisRange._1 <= other._1) && (thisRange._2 < other._2)
  def intersectsRight(other: (Long, Long)): Boolean =
    (thisRange._1 < other._2) && (thisRange._2 > other._1)
  def intersectsLeft(other: (Long, Long)): Boolean =
    (other._1 < thisRange._2) && (other._2 > thisRange._1)

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

  val seedRanges: Array[(Long, Long)] =
    val rangeLimits = lines(0).drop(7).split(' ').map(_.toLong)
    (for i <- 0 until rangeLimits.length / 2
    yield (rangeLimits(i * 2), rangeLimits(i * 2) + rangeLimits(i * 2 + 1))
    ).toArray

  var lowest: Long = Long.MaxValue

  def processInputRanges(inputRanges: Array[(Long, Long)], transformers: Vector[RangePair]): Array[(Long, Long)] =
    var inputRangesStack = inputRanges
    var outputRanges: Array[(Long, Long)] = Array.empty

    while inputRangesStack.nonEmpty do
      for (transformer, i) <- transformers.zipWithIndex do
        println(s"Category ${i + 1}")
        inputRangesStack.foreach(inputRange =>
          if transformer._2 contains inputRange then
            println(s"Fully covered: $transformer, $inputRange")
            outputRanges = outputRanges :+ (
              transformer._1._1 - transformer._2._1 + inputRange._1,
              transformer._1._1 - transformer._2._1 + inputRange._2
            )
            inputRangesStack = inputRangesStack.filterNot(_ == inputRange)
          else if (transformer._2 intersectsRight inputRange) || (transformer._2 intersectsLeft inputRange) then
            println(s"Partial overlap: $transformer, $inputRange")
            val intersectionStart = Math.max(transformer._2._1, inputRange._1)
            val intersectionEnd = Math.min(transformer._2._2, inputRange._2)

            if intersectionStart > inputRange._1 then
              outputRanges = outputRanges :+ (inputRange._1, intersectionStart - 1)

            outputRanges = outputRanges :+ (
              transformer._1._1 - transformer._2._1 + intersectionStart,
              transformer._1._1 - transformer._2._1 + intersectionEnd
            )

            if intersectionEnd < inputRange._2 then
              outputRanges = outputRanges :+ (intersectionEnd + 1, inputRange._2)

            inputRangesStack = inputRangesStack.filterNot(_ == inputRange)
          else 
            println(s"Suitable source range for $inputRange not fuond")
            outputRanges = outputRanges :+ inputRange
            inputRangesStack = inputRangesStack.filterNot(_ == inputRange)
        )
    outputRanges

  var ranges = seedRanges
  for (category <- categories) do
    val result = processInputRanges(ranges, category)
    ranges = result

    println(s"Lowest low: ${result.map(_._1).min}")
