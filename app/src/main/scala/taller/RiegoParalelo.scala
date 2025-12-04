package taller
import ModeloRiego._
import RiegoSecuencial._
import scala.collection.parallel.CollectionConverters._

object RiegoParalelo {

  def costoRiegoFincaPar(f: Finca, pi: ProgRiego): Int = {
    val tiempos = tIR(f, pi)
    val n = f.length

    (0 until n).par.map { i =>
      val ti = tiempos(i)
      val ts = tsup(f, i)
      val tr = treg(f, i)
      val p = prio(f, i)

      if (ts - tr >= ti)
        ts - (ti + tr)
      else
        p * ((ti + tr) - ts)
    }.sum
  }

  def costoMovilidadPar(f: Finca, pi: ProgRiego, d: Distancia): Int = {
    val n = f.length
    if (n <= 1) 0
    else {
      (0 until (n - 1)).par.map { j =>
        val from = pi(j)
        val to = pi(j + 1)
        d(from)(to)
      }.sum
    }
  }

  def generarProgramacionesRiegoPar(f: Finca): Vector[ProgRiego] = {
    val n = f.length

    def permutar(restantes: Vector[Int]): Vector[ProgRiego] = {
      if (restantes.isEmpty) Vector(Vector())
      else {
        if (restantes.length <= 3) {
          restantes.indices.toVector.flatMap { idx =>
            val elem = restantes(idx)
            val sinElem = restantes.take(idx) ++ restantes.drop(idx + 1)
            val permsSub = permutar(sinElem)
            permsSub.map(p => elem +: p)
          }
        } else {
          // Paralelizamos sobre la elección del siguiente elemento
          restantes.indices.toVector.par.flatMap { idx =>
            val elem = restantes(idx)
            val sinElem = restantes.take(idx) ++ restantes.drop(idx + 1)
            val permsSub = permutar(sinElem)
            permsSub.map(p => elem +: p)
          }.toVector
        }
      }
    }
    permutar((0 until n).toVector)
  }

  def ProgramacionRiegoOptimoPar(f: Finca, d: Distancia): (ProgRiego, Int) = {
    val todas = generarProgramacionesRiegoPar(f)

    todas.par
      .map { pi =>
        val costoTotal =
          costoRiegoFincaPar(f, pi) + costoMovilidadPar(f, pi, d)
        (pi, costoTotal)
      }
      .reduceOption { (a: (ProgRiego, Int), b: (ProgRiego, Int)) =>
        val cA = a._2
        val cB = b._2
        if (cA <= cB) a else b
      }
      .getOrElse((Vector(), 0))
  }
}