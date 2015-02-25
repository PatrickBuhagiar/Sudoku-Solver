package mt.edu.um

class Sudoku(val grid: List[List[Int]]) {

	def row (row: Int): Set[Int] = grid.apply(row).toSet.-(0)
	def column(col: Int): Set[Int] = {
	  def C[col](xs: List[List[col]], index: Int): List[col] =
	    xs map (row => row(index))
	C(grid,col).toSet.-(0)
	}
	def block(block: Int): Set[Int] = { 
		def list: List[List[Int]] = grid.grouped(3).toList((block/3) ).map(_.grouped(3).toList((block%3)))
	list.flatten.toSet.-(0)
	}
	override def toString() = {
	  grid.map(_.mkString(" ")).mkString("\n").replace("0", "_")
	  
	}	
}

