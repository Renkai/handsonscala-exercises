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

  /**
   * +-------+-------+-------+
   * | 3 1 6 | 5 7 8 | 4 9 2 |
   * | 5 2 9 | 1 3 4 | 7 6 8 |
   * | 4 8 7 | 6 2 9 | 5 3 1 |
   * +-------+-------+-------+
   * | 2 6 3 |   1   |   8   |
   * | 9 7 4 | 8 6 3 |     5 |
   * | 8 5 1 |   9   | 6     |
   * +-------+-------+-------+
   * | 1 3   |       | 2 5   |
   * |       |       |   7 4 |
   * |     5 | 2   6 | 3     |
   * +-------+-------+-------+
   */
  def renderSudoku(grid: Array[Array[Int]]) = {
    val rows = grid.map {
      row =>
        val rowStr = row.grouped(3).map {
          cell =>
            cell.map(num => if (num == 0) " " else num.toString)
              .mkString(" ", " ", " ")
        }
          .mkString("|", "|", "|")
        rowStr
    }
    rows.grouped(3).map {
      row3 => row3.mkString("\n")
    }.mkString("+-------+-------+-------+\n", "\n+-------+-------+-------+\n", "\n+-------+-------+-------+\n")
  }

  def main(args: Array[String]): Unit = {
    val result = renderSudoku(Array(
      Array(3, 1, 6, 5, 7, 8, 4, 9, 2),
      Array(5, 2, 9, 1, 3, 4, 7, 6, 8),
      Array(4, 8, 7, 6, 2, 9, 5, 3, 1),

      Array(2, 6, 3, 0, 1, 0, 0, 8, 0),
      Array(9, 7, 4, 8, 6, 3, 0, 0, 5),
      Array(8, 5, 1, 0, 9, 0, 6, 0, 0),

      Array(1, 3, 0, 0, 0, 0, 2, 5, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 7, 4),
      Array(0, 0, 5, 2, 0, 6, 3, 0, 0)
    ))

    println(result)
  }

}
