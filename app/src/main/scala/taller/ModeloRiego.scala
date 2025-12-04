package taller
import scala.util.Random

object ModeloRiego {
  type Tablon = (Int, Int, Int)
  type Finca = Vector[Tablon]
  type Distancia = Vector[Vector[Int]]
  type ProgRiego = Vector[Int]
  type TiempoInicioRiego = Vector[Int]

  def tsup(f: Finca, i: Int): Int =
    f(i)._1

  def treg(f: Finca, i: Int): Int =
    f(i)._2

  def prio(f: Finca, i: Int): Int =
    f(i)._3

  private val random = new Random()

  def fincaAlAzar(long: Int): Finca = {
    Vector.fill(long)(
      (
        random.nextInt(long * 2) + 1,
        random.nextInt(long) + 1,
        random.nextInt(4) + 1
      )
    )
  }

  def distanciaAlAzar(long: Int): Distancia = {
    val v = Vector.fill(long, long)(random.nextInt(long * 3) + 1)
    Vector.tabulate(long, long) { (i, j) =>
      if (i < j) v(i)(j)
      else if (i == j) 0
      else v(j)(i)
    }
  }
}
