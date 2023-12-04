@main def run =
  val lines = scala.io.Source.fromFile("./data.txt").getLines.toVector

  var sum = 0

  for line <- lines do
    val wn = line
      .split(':')(1)
      .split('|')(0)
      .split(' ')
      .filter(_.nonEmpty)
      .map(_.toInt)
      .toSet
    val yn = line.split('|')(1).split(' ').filter(_.nonEmpty).map(_.toInt).toSet
    val matches = for num <- yn if wn contains num yield num
    val toAdd = scala.math.pow(2, matches.size - 1).toInt
    if toAdd > 0 then sum += toAdd

  println(sum)
