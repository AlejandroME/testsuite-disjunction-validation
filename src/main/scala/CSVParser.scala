import java.io.FileNotFoundException

import scala.io.Source
import scalaz.{-\/, \/-, \/}

case class Datos(utc: String,
                         frUUID: String,
                         callsign: String,
                         latitud: String,
                         longitud: String,
                         dataTypeInternoFR: String,
                         altitud: String,
                         velocidadKts: String,
                         heading: String,
                         velocidadVertical: String,
                         idInternoFR: String,
                         fr24ReceiverID: String,
                         identificadorDatosInternoFR24: String,
                         offset: String)

object CSVParser{

  def main(args: Array[String]) {
    println(leerCSV("assets/EIETJ.csv"))
  }

  //TODO notación infija, explicar prefija
  def leerCSV(ruta: String): String \/ List[Datos] = {

    try{
      \/-(Source.fromFile(ruta).getLines().map(x => parsear(x.toString)).toList)
    }
    catch {
      case _: FileNotFoundException => -\/("No se ha encontrado el archivo especificado")
      case _: Exception => -\/("Ha ocurrido un error al parsear el archivo.")
    }

  }

  def filtrar(fecha: String, datos: List[Datos]): String \/ List[Datos] = {
      \/-(datos.filter(_.utc == fecha))
  }

  private def parsear(linea: String): Datos = {
    val Array(utc,
      frUUIDing,
      callsign,
      latitud,
      longitud,
      dataTypeInternoFR,
      altitud,
      velocidadKts,
      heading,
      velocidadVertical,
      idInternoFR,
      fr24ReceiverID,
      identificadorDatosInternoFR24,
      offset) = linea.split(",")

    Datos(utc,
      frUUIDing,
      callsign,
      latitud,
      longitud,
      dataTypeInternoFR,
      altitud,
      velocidadKts,
      heading,
      velocidadVertical,
      idInternoFR,
      fr24ReceiverID,
      identificadorDatosInternoFR24,
      offset)
  }

}