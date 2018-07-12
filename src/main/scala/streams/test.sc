case class Car(model: String, year: Int) {
}

val honda = Car("honda", 2014)
honda.model

class Tesla(charge: Int) extends Car("tesla", 2018) {
  def altcompare(other: Car): Boolean = year > other.year
}

class Bike(year: Int) {
  def compare(other: Car) = other.year < year
}
val x = Vector(1)
x.indexOf(1)
x.indexOf(3)
x.indexWhere(_ == 1)
x.indexWhere(_ == 3)
1 == List()
1 == List()

case class Pos(row: Int, col: Int) {
  /** The position obtained by changing the `row` coordinate by `d` */
  def deltaRow(d: Int): Pos = copy(row = row + d)

  /** The position obtained by changing the `col` coordinate by `d` */
  def deltaCol(d: Int): Pos = copy(col = col + d)

  override def equals(obj: scala.Any): Boolean = obj match {
    case Pos(r, c) => row == r && col == c
    case _ => false
  }
}

val p = new Pos(1, 1)
val c = new Pos(3, 3)
p == c
val p2 = new Pos(1, 1)
p == p2
