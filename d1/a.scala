@main def run =
    val source = scala.io.Source.fromFile("./data.txt")
    val lines: Vector[String] = try source.getLines.toVector finally source.close()
    
    val sum = (for line <- lines yield
        val s = (for char <- line
            if char.isDigit
        yield char)
        s"${s(0)}${s(s.length - 1)}".toInt
        ).sum

    println(sum)


