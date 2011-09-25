package Matrix

import Matrix._
import Row._

object Matrix {

  /**A convenient alias */
  type Matrix = List[Row]

  def identity(dimension: Int) = for(i <- List.range(0, dimension)) yield for(j <- List.range(0, dimension)) yield if(i == j) 1.0 else 0.0

  def apply(rowCount: Int, colCount: Int)(f: (Int, Int) => Double) = (
    for (i <- 1 to rowCount) yield
      (for (j <- 1 to colCount) yield f(i, j)).toList
    ).toList

  def transpose(m: Matrix): Matrix =
    if (m.head.isEmpty) Nil else m.map(_.head) :: transpose(m.map(_.tail))

  def mXv(m: Matrix, v: Row) = m.map {
    dotProd(_, v)
  } reduceLeft (_ + _)


  def mXm(m1: Matrix, m2: Matrix) =
    for (m1row <- m1) yield
      for (m2col <- transpose(m2)) yield
        dotProd(m1row, m2col)

  def rowCount(m: Matrix) = m.length

  def colCount(m: Matrix) = m.head.length

  /**effectively add RichMatrix methods to List[List[Double]] */
  implicit def toRichMatrix(m: List[List[Double]]): RichMatrix = new RichMatrix(m)


}

/**Methods that are added to List[List[Double]] by an implicit conversion */
case class RichMatrix(m: List[List[Double]]) {

  def T = transpose(m)

  def *(that: RichMatrix) = mXm(this.m, that.m)

  def *(that: Row) =
    for (col <- this.m) yield
      dotProd(col, that)

  def apply(i: Int, j: Int) = m(i)(j)

  def rowCount = m.length

  def colCount = m.head.length

  def toStr = "n" + m.map {
    _.map {
      "t" + _
    }.reduceLeft(_ + _) + "n"
  }.reduceLeft(_ + _)

  def rotateX(ang: Double) = {
    val angle = ang / 180 * math.Pi
    this.*(RichMatrix(List(List(1., 0., 0., 0.),
      List(0., math.cos(angle), -math.sin(angle), 0.),
      List(0., math.sin(angle), math.cos(angle), 0.),
      List(0., 0., 0., 1.))))
  }

  def rotateY(ang: Double) = {
    val angle = ang / 180 * math.Pi
    this.*(RichMatrix(List(List(math.cos(angle), 0., math.sin(angle), 0.),
      List(0., 1., 0., 0.),
      List(-math.sin(angle), 0., math.cos(angle), 0.),
      List(0., 0., 0., 1.))))
  }

  def rotateZ(ang: Double) = {
    val angle = ang / 180 * math.Pi
    this.*(RichMatrix(List(List(math.cos(angle), -math.sin(angle), 0., 0.),
      List(math.sin(angle), math.cos(angle), 0., 0.),
      List(0., 0., 1., 0.),
      List(0., 0., 0., 1.))))
  }

  def scale(x: Double = 1.0, y: Double = 1.0, z: Double = 1.0) = {
    this.*(RichMatrix(List(List(x, 0., 0., 0.),
                          List(0., y, 0., 0.),
                          List(0., 0., z, 0.),
                          List(0., 0., 0., 1.))))
  }

  def translate(x: Double = 0.0, y: Double = 0.0, z: Double = 0.0) = {
    this.*(RichMatrix(List(List(1., 0., 0., x),
                          List(0., 1., 0., y),
                          List(0., 0., 1., z),
                          List(0., 0., 0., 1.))))
  }
}
