@main def run =
  val lines = scala.io.Source.fromFile("./data.txt").getLines.toVector

  val matchesPerLine = lines.map(line =>
    val wn = line
      .split(':')(1)
      .split('|')(0)
      .split(' ')
      .filter(_.nonEmpty)
      .map(_.toInt)
      .toSet
    val yn = line.split('|')(1).split(' ').filter(_.nonEmpty).map(_.toInt).toSet
    val matches = yn.count(wn.contains)
    matches
  )

  var numOfCards = 0

  def getNbrScratchCardsFromLine(index: Int): Int =
    val matches = matchesPerLine(index)
    var toReturn = matches
    for i <- 1 to matches do toReturn += getNbrScratchCardsFromLine(index + i)

    toReturn

  for index <- matchesPerLine.indices do
    numOfCards += 1 + getNbrScratchCardsFromLine(index)

  println(numOfCards)
