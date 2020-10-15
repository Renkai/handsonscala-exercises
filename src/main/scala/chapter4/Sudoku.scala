package chapter4

object Sudoku {
  def isValidSudoku_origin(grid: Array[Array[Int]]): Boolean = {
    !Range(0, 9).exists { i =>
      val row = Range(0, 9).map(grid(i)(_))
      val col = Range(0, 9).map(grid(_)(i))
      val square = Range(0, 9).map(j => grid((i % 3) * 3 + j % 3)((i / 3) * 3 + j / 3))

      row.distinct.length != row.length ||
        col.distinct.length != col.length ||
        square.distinct.length != square.length
    }
  }

  /**
   * Exercise: Modify the def isValidSudoku method we defined in this chapter to allow testing the validity of
   * partially-filled Sudoku grids, with un-filled cells marked by the value 0.
   *
   * @param grid
   * @return
   */
  def isValidSudoku(grid: Array[Array[Int]]): Boolean = {
    !Range(0, 9).exists { i =>
      val row = Range(0, 9).map(grid(i)(_)).filter(_ != 0)
      val col = Range(0, 9).map(grid(_)(i)).filter(_ != 0)
      val square = Range(0, 9).map(j => grid((i % 3) * 3 + j % 3)((i / 3) * 3 + j / 3)).filter(_ != 0)

      row.distinct.length != row.length ||
        col.distinct.length != col.length ||
        square.distinct.length != square.length
    }
  }

}
