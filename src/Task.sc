def pascal(c: Int, r: Int): Int = {
  if (c == 0 || c == r) 1
  else pascal(c - 1, r - 1) + pascal(c, r - 1)
}

def balance(chars: List[Char]): Boolean = {
  def tailBalance(chars: List[Char], count: Int): Boolean = {
    if (chars.isEmpty) count == 0
    else {
      val stepCount = if (chars.head == '(') count + 1
      else if (chars.head == ')') count - 1
      else count
      if (stepCount < 0) return false
      else tailBalance(chars.tail, stepCount)
    }
  }
  tailBalance(chars, 0)
}

balance("(if (zero? x) max (/ 1 x))".toList)
balance("(-:-)$-)(-%".toList)
