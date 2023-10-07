import Direction._

case class Cell(x: Int, y: Int) {

  // assume Y axis points down
  val move: Direction => Cell =
    _ match
      case TOP => copy(y = y - 1)
      case RIGHT => copy(x = x + 1)
      case BOTTOM => copy(y = y + 1)
      case LEFT => copy(x = x - 1)

  def isInside(minx: Int, miny: Int, maxx: Int, maxy: Int): Boolean =
    minx <= x && miny <= y && x < maxx && y < maxy
    // minx < x && miny < y && x < maxx-1 && y < maxy-1

  def isInside(w: Int, h: Int): Boolean =
    isInside(0, 0, w, h)
}
