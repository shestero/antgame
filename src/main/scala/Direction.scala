enum Direction {
  case LEFT, TOP, RIGHT, BOTTOM

  def clockwise(): Direction = this match
    case LEFT => TOP
    case TOP => RIGHT
    case RIGHT => BOTTOM
    case BOTTOM => LEFT

  def contraClockwise(): Direction = this match
    case LEFT => BOTTOM
    case BOTTOM => RIGHT
    case RIGHT => TOP
    case TOP => LEFT
}

object Direction {
  val clockwise: Direction => Direction = _.clockwise()
  val contraClockwise: Direction => Direction = _.contraClockwise()
}