def extractInteger(s: String, colorIndex: Int): Int =
  var i = colorIndex
  var reversedNumber = ""
  while i >= 0 && s(i).isDigit do
    reversedNumber += s(i)
    i -= 1

  reversedNumber.reverse.toInt

def parseLine(line: String): (Int, Array[Array[Int]]) =
  val id = line.drop(5).split(':')(0).toInt
  val revealedSets: Array[String] = line.split(": ")(1).split(';')
  (
    id,
    for set <- revealedSets yield
      var innerArray = Array.fill(3)(0)

      if set contains "red" then
        innerArray(0) = extractInteger(set, set.indexOf("red") - 2)
      if set contains "green" then
        innerArray(1) = extractInteger(set, set.indexOf("green") - 2)
      if set contains "blue" then
        innerArray(2) = extractInteger(set, set.indexOf("blue") - 2)

      innerArray
  )

@main def run =
  val lines = scala.io.Source.fromFile("./data.txt").getLines.toVector

  var powers: Vector[Int] = Vector()

  for line <- lines do
    val (id, gameSets) = parseLine(line)
    val lowestRGB = Array.fill(3)(0)
    for set <- gameSets do
      for (nbrOfColor, i) <- set.zipWithIndex do
        if nbrOfColor > lowestRGB(i) then lowestRGB(i) = nbrOfColor

    powers = powers :+ (lowestRGB(0) * lowestRGB(1) * lowestRGB(2))

  println(powers.sum)
