import scala.collection.immutable.NumericRange.Exclusive as LRange
type RangePair = ((Long, Long), (Long, Long))

def getRangePair(line: String): RangePair =
  val parts = line.split(' ').map(_.toLong)
  ((parts(0), parts(0) + parts(2)), (parts(1), parts(1) + parts(2)))

@main def run =
  val lines = scala.io.Source.fromFile("./data.txt").getLines.toVector

  var categories: Vector[Vector[RangePair]] = Vector.empty
  var categoryIndex = 0

  def addToCategories(rm: RangePair): Unit =
    if categoryIndex >= categories.length then
      categories = categories :+ Vector.empty
    categories =
      categories.updated(categoryIndex, categories(categoryIndex) :+ rm)

  for line <- lines.drop(3) do
    if line.isBlank then categoryIndex += 1
    else if !line(0).isDigit then {} else addToCategories(getRangePair(line))

  val seedRanges: Array[LRange[Long]] =
    val rangeLimits = lines(0).drop(7).split(' ').map(_.toLong)
    (for i <- 0 until rangeLimits.length / 2
    yield rangeLimits(i * 2) until rangeLimits(i * 2) + rangeLimits(
      i * 2 + 1
    )).toArray

  def updateValueFromRangePair(rp: RangePair, cv: Long): Long =
    if cv >= rp._2._1 && cv < rp._2._2 then rp._1._1 + -1 * (rp._2._1 - cv)
    else cv

  var lowest: Long = Long.MaxValue

  seedRanges.foreach(seeds =>
    println(s"Getting started on range $seeds")
    var precision = 1
    for i <- 0 until seeds.size by precision do
      var currentValue = seeds(i)
      for category <- categories do
        var found = false
        category.foreach(rangePair =>
          val newValue = updateValueFromRangePair(rangePair, currentValue)
          if newValue != currentValue && !found then
            found = true
            currentValue = newValue
        )
      if currentValue < lowest then lowest = currentValue
  )

  println(lowest)
