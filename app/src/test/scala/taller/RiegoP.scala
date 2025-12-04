package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RiegoP extends AnyFunSuite {
  import ModeloRiego._

  test("tIR calcula correctamente los tiempos de inicio para un ejemplo sencillo") {
    val f: Finca = Vector(
      (10, 3, 1), // T0
      (8, 2, 1),  // T1
      (6, 1, 1)   // T2
    )

    // Programación: primero T1, luego T2, luego T0
    val pi: ProgRiego = Vector(1, 2, 0)
    val esperado: TiempoInicioRiego = Vector(3, 0, 2)
    val obtenido = RiegoSecuencial.tIR(f, pi)
    assert(obtenido === esperado)
  }

  test("costoRiegoTablon coincide con la definición para un caso sin sufrimiento") {
    val f: Finca = Vector(
      (10, 3, 4) // ts = 10, tr = 3, p = 4
    )
    val pi: ProgRiego = Vector(0)
    val costo = RiegoSecuencial.costoRiegoTablon(0, f, pi)
    assert(costo === 7)
  }

  test("costoRiegoTablon coincide con la definición para un caso con sufrimiento") {
    val f: Finca = Vector(
      (4, 3, 2) // ts = 4, tr = 3, p = 2
    )
    val pi: ProgRiego = Vector(0)
    succeed
  }

  test("costoRiegoFinca suma los costos de todos los tablones") {
    val f: Finca = Vector(
      (10, 3, 1), // T0
      (5, 2, 1)   // T1
    )
    val pi: ProgRiego = Vector(0, 1)
    val esperado = 7 + 0
    val obtenido = RiegoSecuencial.costoRiegoFinca(f, pi)
    assert(obtenido === esperado)
  }

  test("costoMovilidad calcula correctamente la suma de distancias") {
    val f: Finca = Vector(
      (10, 3, 1),
      (5, 2, 1),
      (6, 1, 1)
    )
    val d: Distancia = Vector(
      Vector(0, 2, 4),
      Vector(2, 0, 3),
      Vector(4, 3, 0)
    )
    val pi: ProgRiego = Vector(0, 2, 1)
    val esperado = 7
    val obtenido = RiegoSecuencial.costoMovilidad(f, pi, d)
    assert(obtenido === esperado)
  }

  test("generarProgramacionesRiego genera todas las permutaciones posibles") {
    val f: Finca = Vector(
      (1, 1, 1),
      (1, 1, 1),
      (1, 1, 1)
    )

    val programaciones = RiegoSecuencial.generarProgramacionesRiego(f)

    assert(programaciones.size === 6)
    assert(programaciones.toSet === Set(
      Vector(0, 1, 2),
      Vector(0, 2, 1),
      Vector(1, 0, 2),
      Vector(1, 2, 0),
      Vector(2, 0, 1),
      Vector(2, 1, 0)
    ))
  }

  test("ProgramacionRiegoOptimo encuentra una programación de costo mínimo") {
    val f: Finca = Vector(
      (5, 2, 1), // T0
      (5, 2, 1)  // T1
    )
    val d: Distancia = Vector(
      Vector(0, 1),
      Vector(1, 0)
    )

    val (piOpt, costoOpt) = RiegoSecuencial.ProgramacionRiegoOptimo(f, d)
    val pi1 = Vector(0, 1)
    val c1 =
      RiegoSecuencial.costoRiegoFinca(f, pi1) +
        RiegoSecuencial.costoMovilidad(f, pi1, d)

    val pi2 = Vector(1, 0)
    val c2 =
      RiegoSecuencial.costoRiegoFinca(f, pi2) +
        RiegoSecuencial.costoMovilidad(f, pi2, d)

    val minimo = math.min(c1, c2)

    assert(costoOpt === minimo)
    assert(piOpt == pi1 || piOpt == pi2)
  }

  test("versión paralela y secuencial de ProgramacionRiegoOptimo dan el mismo costo") {
    val f: Finca = Vector(
      (10, 3, 4),
      (5, 3, 3),
      (2, 2, 1)
    )
    val d: Distancia = Vector(
      Vector(0, 2, 4),
      Vector(2, 0, 3),
      Vector(4, 3, 0)
    )

    val (_, costoSeq) = RiegoSecuencial.ProgramacionRiegoOptimo(f, d)
    val (_, costoPar) = RiegoParalelo.ProgramacionRiegoOptimoPar(f, d)

    assert(costoSeq === costoPar)
  }
}