package mt.edu.um

class SingeltonSolver {

  def solve(sudoku: Sudoku): Sudoku = {

    def hypothesis(r: Int, c: Int): Set[Int] = Set(1,2,3,4,5,6,7,8,9).diff(sudoku.row(r)).diff(sudoku.column(c)).diff(sudoku.block((c/3) + (r/3)*3))
    
    def allHypothesis(): List[(Int, Int, Set[Int])] = {
     def CreateTuple(a: (Int,Int)):(Int,Int, Set[Int]) = (a._1,a._2,hypothesis(a._1,a._2))
     def ZipThis(xs:List[Int]):List[(Int,Int)] = {
       val x:List[(Int,Int)]=xs.zipWithIndex
       return x filter (_._1 == 0) }
    
     def RowIndex(a:Int, b:List[(Int,Int)]):List[(Int,Int)] = {
       val x = List.unzip(b)
       val y = List.make(9, a)
       val z:List[(Int,Int)]=y zip x._2 
       return z}
     def function(x:(List[Int],Int)):List[(Int,Int)] = RowIndex(x._2,ZipThis(x._1))
     def ZipAllItems(xs:List[List[Int]]):List[(Int,Int)]={
       val x=xs.zipWithIndex
       val z:List[List[(Int,Int)]] = x.map(x => function(x))
       z.flatten 
     }
    ZipAllItems(sudoku.grid).map(x=>CreateTuple(x))
    }
    
    def step(): Sudoku ={
     def CardinalityOne(x:List[(Int, Int, Set[Int])]): List[(Int, Int, Set[Int])]= x filter (_._3.size == 1) 
     val x = sudoku.grid.toList
     val y = CardinalityOne(allHypothesis())
     val a= y.foldLeft(x){case (z,(i,j,k:Set[Int])) =>z.updated(i, x(i).updated(j,k.head))}
     new Sudoku(a)
    }
    
    def complete(): Boolean = if ((allHypothesis() filter (_._3.size == 1)) == List()) return true else false

    if (complete()) sudoku else solve(step())
  }
}

object Solver{
	def main(args: Array[String]){
		 val ex1 = new Sudoku(List( 
		     List(0, 0, 5, 0, 0, 6, 3, 0, 0),
		     List(0, 0, 0, 0, 0, 0, 4, 0, 0),
		     List(9, 8, 0, 7, 4, 0, 0, 0, 5),
		     List(1, 0, 0, 0, 7, 0, 9, 0, 0),
		     List(0, 0, 9, 5, 0, 1, 6, 0, 0),
		     List(0, 0, 8, 0, 2, 0, 0, 0, 7),
		     List(6, 0, 0, 0, 1, 8, 0, 9, 3),
		     List(0, 0, 1, 0, 0, 0, 0, 0, 0),
		     List(0, 0, 4, 2, 0, 0, 5, 0, 0)
		     ))
		val ex2 = new SingeltonSolver 
		println(ex2.solve(ex1))
	}
}