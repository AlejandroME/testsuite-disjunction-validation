import java.net.URL

import org.scalatest._

import scala.io.Source

class EitherSpec extends FlatSpec with Matchers {

  def obtenerURL(url: URL): Either[String, Source] = {
    if (url.getHost.contains("google")) Left("Url Bloqueada. Por favor comuníquese con su administrador")
    else Right(Source.fromURL(url))
  }

  "Either" should "Tener proyección derecha" in {
    val success = Right("Exito!")

    success.isRight
    !success.isLeft
  }


  "Filtro de internet" should "bloquear una URL con dominio 'google'" in {

    val respuesta = obtenerURL(new URL("http://plus.google.com"))

    respuesta should be ('left)
  }

  it should "permitir cargar una URL con dominio 'abc.xyz'" in {

    val respuesta = obtenerURL(new URL("http://abc.xyz"))

    respuesta should be ('right)
  }

  it should "mostrar la cantidad de líneas leídas del fuente de http://abc.xyz (6 líneas)" in {
    val respuesta = obtenerURL(new URL ("http://abc.xyz"))

    //TODO Map devuelve un Either, no devuelve la proyección con la que estamos trabajando
    val lineas: Either[String, Int] = respuesta.right.map(_.getLines().size)

    //6 líneas cuando se hizo ésta prueba
    lineas should be (Right(6))
  }

  it should "bloquear la carga al intentar contar las líneas del fuente de http://plus.google.com" in {
    val respuesta = obtenerURL(new URL ("http://plus.google.com"))
    val lineas = respuesta.right.map(_.getLines().size)

    //Bloqueo de la página, retorno de proyección de error a pesar de que se está trabajando con la derecha
    lineas should be ('left)
  }

  ignore should " no compilar al usar un for-comprehension sin envolver en una proyección" in {
    /*val instruccion =
        for {
           sitio1 <- obtenerURL(new URL("http://abc.xyz")).right
           sitio2 <- obtenerURL(new URL("http://www.latinchat.com")).right
           /*
              El problema está aquí: Se retorna un Either en vez de una RightProjection.
              Toca envolver nuevamente el resultado en un Right para que así no se introduzca un "map"
              (como se hace en este momento, al ser Either) sino un flatMap en la proyección correcta
              Forma: Right(sitio1.getLines().size).right
            */
           lineasSitio1 <- sitio1.getLines().size
           lineasSitio2 <- sitio2.getLines().size
        }
    yield lineasSitio1 + lineasSitio2

    //TODO valor inventado, no se ha revisado cuanto da la operación
    instruccion should be (Right(5))*/
  }




}
