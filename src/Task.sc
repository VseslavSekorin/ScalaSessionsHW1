def pascal(c: Int, r: Int): Int = {
  if (c < 0 || r < 0 || r < c) throw new IllegalArgumentException()
  if (c == 0 || c == r) 1
  else pascal(c - 1, r - 1) + pascal(c, r - 1)
}

def balance(chars: List[Char]): Boolean = {
  def tailBalance(chars: List[Char], count: Int): Boolean = {
    if (chars.isEmpty) count == 0
    else {
      val stepCount = chars.head match {
        case '(' => count + 1
        case ')' => count - 1
        case _ => count
      }
      if (stepCount < 0) false
      else tailBalance(chars.tail, stepCount)
    }
  }
  tailBalance(chars, 0)
}

balance("(if (zero? x) max (/ 1 x))".toList)
balance("(-:-)$-)(-%".toList)
