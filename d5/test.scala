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

  def processInputRanges(inputRanges: Array[(Long, Long)], transformers: Vector[RangePair]): Array[(Long, Long)] =
    var inputRangesStack = inputRanges
    var outputRanges: Array[(Long, Long)] = Array.empty
    println(s"Input ranges are ${inputRangesStack.mkString(", ")}")

    while !inputRangesStack.isEmpty do
      var foundSuitingTransformer = false
      val range = inputRangesStack(0)
      transformers.foreach(transformer =>
        if transformer._2 contains range then
          val rangeLength = range._2 - range._1
          val transformedRangeStart = transformer._1._1 - transformer._2._1 + range._1
          outputRanges = outputRanges :+ (transformedRangeStart, transformedRangeStart + rangeLength)
          inputRangesStack = inputRangesStack.filterNot(_ == range)

          foundSuitingTransformer = true
        else if transformer._2 intersectsLeft range then
          // Overlap range
          val overlap = (transformer._2._1, range._2)
          val overlapLength = overlap._2 - overlap._1
          outputRanges = outputRanges :+ (transformer._1._1, transformer._1._1 + overlapLength)
          inputRangesStack = inputRangesStack.filterNot(_ == range)

          // Remaining
          inputRangesStack = inputRangesStack :+ (range._1, transformer._2._1)

          foundSuitingTransformer = true
        else if transformer._2 intersectsRight range then
          // Overlap range
          val overlap = (range._1, transformer._2._1)
          val overlapLength = overlap._2 - overlap._1
          outputRanges = outputRanges :+ (transformer._1._2 - overlapLength, transformer._1._2)
          inputRangesStack = inputRangesStack.filterNot(_ == range)

          // Remaining
          inputRangesStack = inputRangesStack :+ (transformer._2._2, range._2)

          foundSuitingTransformer = true
      )
      if !foundSuitingTransformer then 
        outputRanges = outputRanges :+ range
        inputRangesStack = inputRangesStack.filterNot(_ == range)
    
    println(s"Output ranges are ${outputRanges.mkString(", ")}")
    outputRanges

  var ranges = seedRanges
  for (category, i) <- categories.zipWithIndex do
    println(s"Category ${i + 1}")
    val result = processInputRanges(ranges, category)
    ranges = result

  
  println(s"Lowest low: ${ranges.map(_._1).min}")
