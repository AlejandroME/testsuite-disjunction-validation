import org.scalatest._
import scalaz._
import Scalaz._

class ValidationSpec extends FlatSpec with Matchers {

  "Validation" should "ser un éxito" in {
    val valor = 15.success

    valor shouldBe Success(15)
  }

  it should "ser un fallo" in {
    val valor = "Fallé".failure

    valor shouldBe Failure("Fallé")
  }

  it should "fallar al intentar entrar un menor de edad a la discoteca" in {
    val persona = Persona("Juan Pedro Santamaria", 17, Estado.NoApto, EstadoAlcohol.Sobrio)

    val aptoParaEntrar = for{
      edad <- Validaciones.validarEdad(persona)
      estadoEtilico <- Validaciones.validarEstadoAlcohol(persona)
    }
      yield true

    aptoParaEntrar shouldBe Failure("Menor de edad")
  }

  /*
    Aquí vuelve a retornar error de validaciòn por "Menor de edad", pero no retorna la validaciòn
    por estado etílico, ya que aún no estamos acumulando errores (Sigue siendo fail-fast).
   */
  it should "fallar al intentar entrar un menor de edad prendo a la discoteca" in {
    val persona = Persona("Juan Pedro Santamaria", 17, Estado.NoApto, EstadoAlcohol.Prendo)

    val aptoParaEntrar = for{
      edad <- Validaciones.validarEdad(persona)
      estadoEtilico <- Validaciones.validarEstadoAlcohol(persona)
    }
      yield true

    aptoParaEntrar shouldBe Failure("Menor de edad")
  }

  /*
    Validation acumulativo:

    "findSuccess" favorece a los éxitos por encima de los fallos
   */

  it should "fallar al intentar entrar un menos de edad borracho a la discoteca" in {
    val persona = Persona("Juan Pedro Santamaria", 17, Estado.NoApto, EstadoAlcohol.Borracho)

    val validacionEdad = Validaciones.validarEdad(persona).toValidationNel
    val validacionEstadoAlcohol = Validaciones.validarEstadoAlcohol(persona).toValidationNel

    (validacionEdad findSuccess validacionEstadoAlcohol) shouldBe Failure(NonEmptyList("Menor de edad", "Demasiado mal para entrar"))
  }

  it should "dejar entrar a un mayor de edad sobrio a la discoteca" in {
    val persona = Persona("Juan Pedro Santamaria", 24, Estado.NoApto, EstadoAlcohol.Sobrio)

    /*
      No podemos hacer nuestras validaciones dentro de un for-comprehension ya que allí estamos en un contexto
      monádico. Validation es right-biased, al igual que \/, por lo que en ese contexto tendríamos el objeto
      de la derecha (persona) y no nuestro objeto Validation, el cual combinamos con el operador "findSuccess"
     */

    /*for {
      edad <- Validaciones.validarEdad(persona).toValidationNel
      estadoEtilico <- Validaciones.validarEstadoAlcohol(persona).toValidationNel
    }
      yield edad findSuccess estadoEtilico*/

    val validacionEdad = Validaciones.validarEdad(persona).toValidationNel
    val validacionEstadoAlcohol = Validaciones.validarEstadoAlcohol(persona).toValidationNel

    (validacionEdad findSuccess validacionEstadoAlcohol) shouldBe Success(Persona("Juan Pedro Santamaria", 24, Estado.NoApto, EstadoAlcohol.Sobrio))
  }
}
