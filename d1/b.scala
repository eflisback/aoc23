@main def run =
  val source = scala.io.Source.fromFile("./data.txt")
  val lines: Vector[String] =
    try source.getLines.toVector
    finally source.close()

  val stringDigits = Map(
    "zero" -> 0,
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
  )

  var sum = 0

  for line <- lines do
    var nums: Vector[(Int, Int)] = Vector.empty
    for (stringDigit, digitValue) <- stringDigits do
      val digitPattern =
        s"(?i)$stringDigit"
      val matches = digitPattern.r.findAllMatchIn(line)

      for (m, i) <- matches.zipWithIndex do
        nums = nums :+ (digitValue, m.start + i)

    for (char, i) <- line.zipWithIndex do
      if char.isDigit then nums = nums :+ (char.asDigit, i)

    nums = nums.sortBy(_._2)

    if nums.nonEmpty then
      val s: Int = s"${nums.head._1}${nums.last._1}".toInt
      sum += s

  println(sum)
