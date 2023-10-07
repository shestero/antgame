import scala.util.chaining.*

class AntGame extends Field {

  private val isAntInField: AntState => Boolean = isInside compose { _.position }

  /**
   * Move the given ant into its next state/position.
   * @return the new ant state/position either None if ant escaped out of the field
   */
  private def next(ant: AntState): Option[AntState] =
    ant.step(getAndInvert(ant.position)) pipe Option.apply pipe { _.filter(isAntInField) }

  private val play: Iterator[Unit] =
    Iterator.unfold(AntState.start)(next andThen { _.map(() -> _) })

  def run(): AntGame =
    play
      .zip(Iterator.from(1)) // step counter
      .grouped(10000) // chunk size (for output throttling)
      .flatMap(_.lastOption)
      .map(_._2)
      .map("Step#" + _ + s"\tCount of black cells = ${bitset.size} ...")
      .foreach(println)
      .pipe(_ => this)
}
