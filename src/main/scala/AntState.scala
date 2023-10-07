import Direction._

case class AntState(position: Cell, direction: Direction) {

  def forward(): AntState =
    copy(position = position.move(direction))

  def turn(rotate: Direction => Direction): AntState =
    copy(direction = rotate(direction))

  def step(isBlack: Boolean): AntState =
    turn(if (isBlack) contraClockwise else clockwise).forward()
}

object AntState {
  val start: AntState = AntState(Cell(511,511), Direction.TOP)
}
