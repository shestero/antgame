import java.awt.image.{BufferedImage, IndexColorModel}
import java.io.File
import javax.imageio.ImageIO
import scala.collection.mutable
import scala.util.chaining.*

class Field {
  val w = 1024
  val h = 1024

  val bitset = mutable.BitSet() // (w * h)

  val isInside: Cell => Boolean = _.isInside(w, h)

  given Conversion[Cell, Int] with
    def apply(pos: Cell): Int = pos.x + pos.y * w

  val isBlack: Cell => Boolean = bitset.contains

  val makeBlack: Cell => Unit = bitset.addOne

  val makeWhite: Cell => Unit = bitset.subtractOne

  /**
   * Read the cell state (black or white), then invert this state.
   * @param cell the place where to check and act
   * @return true if the cell was black, false if it was white
   */
  def getAndInvert(cell: Cell): Boolean =
    isBlack(cell) tap { if (_) makeWhite(cell) else makeBlack(cell) }

  /** Save the field into png picture */
  def save(fileName: String = "antway.png"): Field = {
    val v = Array(0.toByte, 0xff.toByte)
    val cm = new IndexColorModel(1, v.length, v, v, v)
    val wr = cm.createCompatibleWritableRaster(w, h)
    val out = new BufferedImage(cm, wr, false, null)
    for {
      x <- 0 until w
      y <- 0 until h
      cell = Cell(x, y)
      a = if (isBlack(cell)) 0 else 255
      p = a << 16 | a << 8 | a
    } {
      out.setRGB(x, y, p)
    }
    val g = out.createGraphics
    g.drawImage(out, 0, 0, null)
    g.dispose
    ImageIO.write(out, "png", new File(fileName))
    println(s"The field was saved into $fileName")
    this
  }
}
