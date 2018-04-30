object SpiralMatrix {
  type Matrix = Array[Array[Int]]

  def spiralMatrix(dim: Int): List[List[Int]] = {
    val initialMatrix: Matrix = Array.fill(dim)(Array.fill(dim)(0))

    @scala.annotation.tailrec
    def spiralTailRec(offset: Int, row: Int, col: Int, size: Int): Unit = {
      if(size == 0) () // Do nothing
      else if(size == 1) initialMatrix(row)(col) =  offset + 1
      else {
        // Fill first row
        for(j <- 0 until size) initialMatrix(row)(col + j) = offset + (j + 1)
        // Fill last column
        for(i <- 0 until size) initialMatrix(row + i)(col + size - 1) = offset + size + i
        // Fill last row
        for(j <- 0 until size) initialMatrix(row + size - 1)(col + j) = offset + 3*size - 2 - j
        // Fill first column
        for(i <- 1 until size) initialMatrix(row + i)(col) = offset + 4*size - 3 - i

        spiralTailRec(offset + 4*(size-1), row + 1, col + 1, size - 2)
      }
    }

    spiralTailRec(0, 0, 0, dim)
    initialMatrix.map(_.toList).toList
  }
}
