import org.scalatest._
import scalaz._
import Scalaz._

class DisjunctionSpec extends FlatSpec with Matchers {

  "Disjunction" should "Tener proyección derecha e izquierda" in {
    val izquierda: Disjunction[String, Nothing] = -\/("Error")
    val derecha: Disjunction[Nothing, String] = \/-("Exito")

    izquierda.isLeft
    derecha.isRight
  }

  it should "retornar un Right al hacer comprehension de dos exitos" in {
    val exito1 = \/-("Exito!")
    val exito2 = \/-("Yo también funcioné! :D!")

    val exito3 = for{
      e1 <- exito1
      e2 <- exito2
    } yield e1 |+| e2

    exito3 shouldBe \/-("Exito!Yo también funcioné! :D!")
  }

  //TODO Test de "Fail-Fast" en un comprehension
  it should "retornar un Left al hacer comprehension de una falla y varios exitos" in {
    val exito1 = \/-("Exito!")
    val falla1 = -\/("Yo no funcioné... :(")
    val exito2 = \/-("Yo si funcioné!")

    val resultado = for{
      e1 <- exito1
      f1 <- falla1
      e2 <- exito2
    } yield (e1, e2)

    resultado shouldBe -\/("Yo no funcioné... :(")
  }

  it should "expresar un Some como un Right del disjunction" in {
    val resultado = Some("Objeto encontrado").toRightDisjunction("Objeto no encontrado")

    resultado shouldBe \/-("Objeto encontrado")
  }

  it should "expresar un None como un Left del disjunction" in {
    val resultado = None.toRightDisjunction("Objeto no encontrado")

    resultado shouldBe -\/("Objeto no encontrado")
  }

  it should "traducir un Disjunction a un Either estándar" in {
    val resultado = Some("Objeto encontrado").toRightDisjunction("Objeto no encontrado").toEither

    resultado shouldBe Right("Objeto encontrado")
  }

  //TODO Parser de CSV

  it should "retornar una lista de objetos de tipo 'DatosMetrojet'" in {
    val listaDatosVueloMetrojet = CSVParser.leerCSV("assets/EIETJ.csv")

    listaDatosVueloMetrojet.map{
      x =>
        x shouldBe a [List[_]]
    }
  }

  it should "retornar una excepción por no existencia del archivo en la ruta especificada" in {
    val listaDatosVueloNoExistente = CSVParser.leerCSV("youcantfindme.csv")

    listaDatosVueloNoExistente shouldBe -\/("No se ha encontrado el archivo especificado")
  }

  it should "retornar una excepción enriquecida (swap)" in {
    val listaDatosVueloNoExistente = CSVParser.leerCSV("youcantfindme.csv").swap.map(x => "El error es: " + x)

    listaDatosVueloNoExistente shouldBe \/-("El error es: No se ha encontrado el archivo especificado")
  }

  it should "retornar una excepción al parsear el archivo CSV por malformación" in {
    val listaDatosVueloDataMala = CSVParser.leerCSV("assets/EIETJ_Wrong.csv")

    listaDatosVueloDataMala shouldBe -\/("Ha ocurrido un error al parsear el archivo.")
  }

  it should "mostrar cuál fue el último registro que se tuvo del avión" in {

    val ultimaHoraVuelo = for {
      informacionVuelo <- CSVParser.leerCSV("assets/EIETJ.csv")
    }
      yield informacionVuelo.last.utc

    ultimaHoraVuelo shouldBe \/-("04:13:21Z.976")
  }

  it should "retornar la altitud al filtrar por una fecha específica" in {
    val datos = for{
      informacionVuelo <- CSVParser.leerCSV("assets/EIETJ.csv")
      /*
        Este comprehension es Monádico, por lo tanto al aplicar .filter estamos devolviendo
        una lista, no un \/- de la lista. de ahí a que tengamos que envolver el .filter en un Right,
        con lo cual tampoco estamos logrando mucho (En tema de "expresividad").
        Si no hacemos esa operación en este caso los tipos no coincidirán.
       */
      informacionPorHora <- \/-(informacionVuelo.filter(_.utc == "04:13:00Z.616"))
    }
      yield informacionPorHora.head.altitud

    datos shouldBe \/-("29750")
  }

  it should "retornar la altitud al filtrar por una fecha específica (monádico)" in {
    val datos = for{
      informacionVuelo <- CSVParser.leerCSV("assets/EIETJ.csv")
      informacionPorHora <- CSVParser.filtrar("04:13:00Z.616", informacionVuelo)
    }
      yield informacionPorHora.head.altitud

    datos shouldBe \/-("29750")
  }

}
