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

  val seeds: Array[Long] =
    lines(0).drop(7).split(' ').map(_.toLong)
  val locations: Array[Long] =
    for seed <- seeds yield
      var currentValue = seed
      for (category, i) <- categories.zipWithIndex do
        var found = false
        category.foreach(rangePair =>
          if currentValue >= rangePair._2._1 && currentValue < rangePair._2._2 && !found
          then
            val newValue =
              rangePair._1._1 + -1 * (rangePair._2._1 - currentValue)
            currentValue = newValue
            found = true
        )
      currentValue

  println(locations.min)
