object PascalTriangle {
  def pascal(row: Int, col: Int): Int = {
    if (col == 0 || col == row) 1
    else pascal(row - 1, col - 1) + pascal(row - 1, col)
  }

  def main() {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row) {
        print(pascal(row, col) + " ")
      }
      println()
    }
  }
}
