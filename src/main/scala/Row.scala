package Matrix

import Row._

object Row{

  /** A convenient alias */
  type Row = List[Double]

  def dotProd(v1:Row,v2:Row) = 
    v1.zip( v2 ).map{ t:(Double,Double) => t._1 * t._2 }.reduceLeft(_ + _)

  def vPlusV(v1:Row,v2:Row) =
    v1.zip( v2 ).map{ t:(Double,Double) => t._1 + t._2 }

  /** effectively add RichRow methods to List[Double] */
  implicit def pimp(v:Row) = new RichRow(v)

}

/** Methods that are added to List[Double] by an implicit conversion */
case class RichRow(v:Row){

  /** dot product */
  def *(that:RichRow)   = dotProd( this.v, that.v )

  /** vector addition */
  def add(that:RichRow) = vPlusV( this.v, that.v )

  /** convert to column vector */
  def T = v.map{ List(_) }

  /** As row matrix */
  def asMatrix = List( v )

}
