// Takes a number - say 4 (n)
// Uses this to create a board which is  4 x 4 (n x n)
def printBoard(board: Seq[Seq[Int]]): Unit = {
  // mkString takes elements in a List and combines them with the specified characters
  // ex. Seq(1, 2, 3).mkString(" ~ ") would output: "1 ~ 2 ~ 3"
  val boardText = board.map(row => row.mkString(", ")).mkString("\n")
  println(boardText)
}

def createBoard(n: Int): Seq[Seq[Int]] = {
  // Empty board
  // Figure out how to create board
  val board = Seq.fill(n)(Seq.fill(n)(0))
  // try to populate first row
  // Make Pusdo code comments
  //printSolution(board)
  board
}

def placeQueen(board: Seq[Seq[Int]], row: Int, col: Int): Seq[Seq[Int]] = {
  // remove is safe logic


  // if(isSafe(board,row,col) & (row < 4 & col < 4))
  // {
  val newRow = board(row).updated(col, 2)
  val newBoard = board.updated(row, newRow)
  //placeQueen(newBoard, row , col )
  newBoard
  // }

}

def placeQueens(n: Int) = {
  // create board - size n
  val board = createBoard(n)
  // call place queens
  var newBoard = board
  // insert looping logic here
  board.zipWithIndex.map { case(row,rowI) =>
    row.map { col =>
      newBoard = placeQueen(newBoard,rowI,col)

    }
  }
  printBoard(newBoard)

}

def isSafe(board: Seq[Seq[Int]], row: Int, col: Int): Boolean = {
  //how to determine if safe?
  val rowCounter = 0
  val colCounter = 0
  var checkBoard = board.exists { boardRow =>
    var checkCol = boardRow.exists { boardSquare =>
      (boardSquare == 2 & ((rowCounter == row) || (colCounter == col)))
    }
    checkCol
  }
  !checkBoard
}






val solvedBoard = placeQueens(4)
