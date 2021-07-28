import com.sun.org.apache.xpath.internal.operations.Or

object ChessBoard extends App {

  type Board = Seq[Seq[String]]

  // Takes a number - say 4 (n)
  // Uses this to create a board which is  4 x 4 (n x n)
  def printBoard(board: Board): Unit = {
    // mkString takes elements in a List and combines them with the specified characters
    // ex. Seq(1, 2, 3).mkString(" ~ ") would output: "1 ~ 2 ~ 3"
    val boardText = board.map(row => row.mkString(", ")).mkString("\n")
    println(boardText)
  }

  def createBoard(n: Int): Board = {
    // Empty board
    // Figure out how to create board
    val board = Seq.fill(n)(Seq.fill(n)("-"))
    // try to populate first row
    // Make Pusdo code comments
    //printSolution(board)
    board
  }

  def markXs(board: Board, row: Int, col: Int): Board = {

    val newRow = board(row).updated(col, "X")
    val newBoard = board.updated(row, newRow)

    newBoard

  }

  def placeQueen(board: Board, row: Int, col: Int): Board = {

    val newRow = board(row).updated(col, "Q")
    var newBoard = board.updated(row, newRow)

    //Place X's where Q can no longer go
    //Navigate whole borads

    def isInvalidSquare(rowI: Int, row:Int,  colI:Int, col:Int): Boolean ={
      checkDiagonals(rowI,row,colI,col) || checkRow(rowI,row,colI,col) || checkCol(rowI,row,colI,col)
    }

    def checkDiagonals(rowI: Int, row:Int, colI:Int, col:Int): Boolean ={
      (0 to newRow.size).exists { i =>
        ((rowI == (row + i) && colI == (col + i)) || (rowI == (row - i) && colI == (col + i)) ||
          (rowI == (row - i) && colI == (col - i)) || (rowI == (row + i) && colI == (col - i))) && (rowI != row && colI != col)
      }

    }

    def checkRow(rowI: Int, row:Int, colI:Int, col:Int): Boolean ={
      rowI == row && colI != col
    }

    def checkCol(rowI: Int, row:Int, colI:Int, col:Int): Boolean ={
      rowI != row && colI == col
    }

    def markInvalidSquares(b: Board): Board = {
      var markedBoard = b
      b.zipWithIndex.flatMap { case (rowL, rowI) =>
        rowL.zipWithIndex.map { case (colL, colI) =>
          if (isInvalidSquare(rowI, row, colI, col)) {
            markedBoard = markXs(markedBoard, rowI, colI)
          }
        }
      }
      markedBoard
    }

    val boardsWMarks = markInvalidSquares(newBoard)

    boardsWMarks
  }

  def countQueens(b: Board): Int = {
    var countQs = 0
    b.zipWithIndex.flatMap { case (rowL, rowI) =>
      rowL.zipWithIndex.map { case (colL, colI) =>
        if(colL == "Q"){
          countQs = countQs + 1
        }
      }
    }
    countQs
  }

  def placeNextQueen(groupOfBoards: Seq[Board], qCount: Int): Seq[Board] = {
    groupOfBoards.flatMap { tmpBoard =>
      tmpBoard.zipWithIndex.flatMap { case (row, rowI) =>
        row.zipWithIndex.filter { case(col,colI) =>
          // check if safe
          isSafe(tmpBoard,rowI,colI)
        }
          .map { case (col, colI) =>

            val newBoard = placeQueen(tmpBoard,rowI,colI)
            // newnewnewborad = to place 3rd queen, and so on
            newBoard
          }
      }

    }.filter { tmpBoard =>
      (countQueens(tmpBoard) == qCount)
    }.distinct
  }

  // down the road - we need to use is safe and this function recurisvesly
  def placeQueens(n: Int): Seq[Board] = {
    // create board - size n

    var qCount = 1

    var boardsWithQueens = placeNextQueen(Seq(createBoard(n)),1)

    while(qCount < n){
      qCount = qCount + 1
      boardsWithQueens = placeNextQueen(boardsWithQueens,qCount)
    }
    // any boards left? when n? - if so then we have valid boards

    boardsWithQueens.distinct

  }

  def isSafe(board: Board, row: Int, col: Int): Boolean = {
    //how to determine if safe?
    var rowCounter = -1
    var colCounter = -1
    var checkBoard = board.exists { boardRow =>
      rowCounter = rowCounter + 1
      var checkCol = boardRow.exists { boardSquare =>
        colCounter = colCounter + 1
        ((boardSquare == "X" || boardSquare == "Q") & ((rowCounter == row) & (colCounter == col)))
      }
      checkCol
    }
    !checkBoard
  }

  // just one queen at the moment
  //
  val solvedBoards = placeQueens(6)
  solvedBoards.map { solvedBoard =>
    printBoard(solvedBoard)
    println("------")
  }

}
