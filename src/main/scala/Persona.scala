import scalaz._
import Scalaz._

object Estado extends Enumeration {val Apto, NoApto = Value}
object EstadoAlcohol extends Enumeration {val Sobrio, Prendo, Borracho = Value}

case class Persona(nombre: String, edad: Int, estadoValidacion: Estado.Value, estado: EstadoAlcohol.Value)

object Validaciones {

  def validarEdad(persona: Persona): Validation[String, Persona] = {
    if (persona.edad < 18) "Menor de edad".failure
    else persona.success
  }

  def validarEstadoAlcohol(persona: Persona): Validation[String, Persona] = {
    if (Set(EstadoAlcohol.Prendo, EstadoAlcohol.Borracho).contains(persona.estado)) "Demasiado mal para entrar".failure
    else persona.success
  }
}