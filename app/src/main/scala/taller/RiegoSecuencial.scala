package taller
import ModeloRiego._

object RiegoSecuencial {

  def tIR(f: Finca, pi: ProgRiego): TiempoInicioRiego = {
    val n = f.length

    // acumulador: (tiemposparciales,tiempoacumulado, indiceenpi)
    def construir(
                   idx: Int,
                   tiempos: TiempoInicioRiego,
                   tiempoActual: Int
                 ): TiempoInicioRiego = {
      if (idx >= n) tiempos
      else {
        val tablon = pi(idx)
        val nuevosTiempos = tiempos.updated(tablon, tiempoActual)
        val nuevoTiempo = tiempoActual + treg(f, tablon)
        construir(idx + 1, nuevosTiempos, nuevoTiempo)
      }
    }

    construir(0, Vector.fill(n)(0), 0)
  }

  def costoRiegoTablon(i: Int, f: Finca, pi: ProgRiego): Int = {
    val tiempos = tIR(f, pi)
    val ti = tiempos(i)
    val ts = tsup(f, i)
    val tr = treg(f, i)
    val p = prio(f, i)

    if (ts - tr >= ti)
      ts - (ti + tr)
    else
      p * ((ti + tr) - ts)
  }

  /** Costo total de riego de la finca f bajo la programación pi. */
  def costoRiegoFinca(f: Finca, pi: ProgRiego): Int = {
    val n = f.length

    def sumar(i: Int, acc: Int): Int =
      if (i >= n) acc
      else sumar(i + 1, acc + costoRiegoTablon(i, f, pi))

    sumar(0, 0)
  }

  /** Costo de movilidad del sistema de riego para la programación pi. */
  def costoMovilidad(f: Finca, pi: ProgRiego, d: Distancia): Int = {
    val n = f.length

    def sumar(j: Int, acc: Int): Int =
      if (j >= n - 1) acc
      else {
        val from = pi(j)
        val to = pi(j + 1)
        val costo = d(from)(to)
        sumar(j + 1, acc + costo)
      }

    sumar(0, 0)
  }

  def generarProgramacionesRiego(f: Finca): Vector[ProgRiego] = {
    val n = f.length

    def permutar(restantes: Vector[Int]): Vector[ProgRiego] = {
      if (restantes.isEmpty) Vector(Vector())
      else {
        restantes.indices.toVector.flatMap { idx =>
          val elem = restantes(idx)
          val sinElem = restantes.take(idx) ++ restantes.drop(idx + 1)
          val permsSub = permutar(sinElem)
          permsSub.map(p => elem +: p)
        }
      }
    }

    permutar((0 until n).toVector)
  }

  def ProgramacionRiegoOptimo(f: Finca, d: Distancia): (ProgRiego, Int) = {
    val todas = generarProgramacionesRiego(f)

    def mejor(
               restantes: Vector[ProgRiego],
               mejorPi: ProgRiego,
               mejorCosto: Int
             ): (ProgRiego, Int) = {
      if (restantes.isEmpty) (mejorPi, mejorCosto)
      else {
        val pi = restantes.head
        val costo =
          costoRiegoFinca(f, pi) + costoMovilidad(f, pi, d)
        if (costo < mejorCosto)
          mejor(restantes.tail, pi, costo)
        else
          mejor(restantes.tail, mejorPi, mejorCosto)
      }
    }

    todas match {
      case Vector() =>
        (Vector(), 0)
      case v =>
        val pi0 = v.head
        val costo0 =
          costoRiegoFinca(f, pi0) + costoMovilidad(f, pi0, d)
        mejor(v.tail, pi0, costo0)
    }
  }
}