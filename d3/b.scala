@main def run = 
    val lines = scala.io.Source.fromFile("./data.txt").getLines.toVector
    
    val matrix: Vector[Vector[Char]] = 
        for line <- lines yield
            (for char <- line yield
                char).toVector

    def detectNumberOnRow(rowIndex: Int, symbolX: Int): Vector[Int] = 
        var adjacentNumbersOnRow: Vector[Int] = Vector()
        val row = matrix(rowIndex)
        var i = 0
        while i < row.length do
            var indexedNumberChars: Vector[(Char, Int)] = Vector()
            while i < row.length && row(i).isDigit do
                indexedNumberChars = indexedNumberChars :+ (row(i), i)
                i += 1
            if indexedNumberChars.exists(charAndIndex =>
                symbolX - 1 to symbolX + 1 contains charAndIndex._2  )  
             then adjacentNumbersOnRow = adjacentNumbersOnRow :+ indexedNumberChars.map(charAndIndex =>
                charAndIndex._1    
            ).mkString.toInt
            i += 1
        adjacentNumbersOnRow
    
    def extractGearRatio(y: Int, x: Int): Int = 
        var ratio = 1
        var totalAdjacent = 0
        for i <- -1 to 1 if 0 until matrix.length contains y + i do
            val factors = detectNumberOnRow(y + i, x)
            val ratioProduct = factors.product
            if ratioProduct != 0 then 
                totalAdjacent += factors.length
                ratio *= ratioProduct
        if totalAdjacent == 2 then ratio else 0

    var sum = 0

    for (row, y) <- matrix.zipWithIndex do
        for (element, x) <- row.zipWithIndex do
            if element == '*' then sum += extractGearRatio(y, x)
    
    println(sum)
    
