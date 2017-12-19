import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import scala.io.StdIn

import scala.util.Random
import scala.language.implicitConversions

object TimeFacts {

  sealed trait Part {
    def getFactPart: String
  }

  case class Sequence(parts: Part*) extends Part {
    override def getFactPart: String =
      parts.map(_.getFactPart).mkString(" ").replace(" \\", "")
  }

  case class Choice(options: Part*) extends Part {
    override def getFactPart: String = options(Random.nextInt(options.length)).getFactPart
  }

  case class Literal(string: String) extends Part {
    override def getFactPart: String = string
  }

  implicit def partFromString(string: String): Part = Literal(string)

  val source = Sequence(
    "Did you know that",
    Choice(
      Sequence("the", Choice("spring", "autumn"), "equinox"),
      Sequence("the", Choice("winter", "summer"), Choice("solstice", "olympics")),
      Sequence("the", Choice("earliest", "latest"), Choice("sunrise", "sunset")),
      Sequence("daylight", Choice("saving", "savings"), "time"),
      Sequence("leap", Choice("day", "year")),
      Sequence("the", Choice("harvest", "super", "blood"), "moon"),
      Choice("Easter", "Toyota Truck Month", "Shark Week")
    ),
    Choice(
      Sequence("happens", Choice("later", "earlier", "at the wrong time"), "every year"),
      Sequence(
        "drifts out of sync with the",
        Choice("sun", "moon", "zodiac", Sequence(Choice("Gregorian", "Mayan", "lunar", "iPhone"), "Calendar")),
      ),
      Sequence("might", Choice("not happen", "happen twice"), "this year")
    ),
    "because of",
    Choice(
      Sequence("time-zone legislation in", Choice("Indiana", "Arizona", "Russia")),
      "a decree by the pope in the 1500s",
      Sequence(
        Choice("precession", "liberation", "nutation", "libation", "eccentricity", "obliquity"),
        "of the",
        Choice("Moon", "Sun", "Earth's axis", "equator", "Prime Meridian", Sequence(Choice("International Date", "Mason-Dixon"), "Line")),
      ),
      "magnetic field reversal",
      Sequence("an arbitrary decision by", Choice("Benjamin Franklin", "Isaac Newton", "FDR"))
    ),
    "\\?",
    "Apparently",
    Choice(
      "it causes a predictable increase in car accidents",
      "that's why we have leap seconds",
      "scientists are really worried",
      Sequence("it was even more extreme during the", Choice("Bronze Age", "Iron Age", "Cretaceous", "1990s")),
      Sequence("there's a proposal to fix it, but it", Choice("will never happen", "actually makes things worse", "is stalled in congress", "might be unconstitutional")),
      "it's getting worst and no one knows why"
    ),
    "\\."
  )

  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem("time-facts")
    implicit val materializer = ActorMaterializer()
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.dispatcher

    val route =
      path("") {
        get {
          complete(HttpEntity(ContentTypes.`application/json`, "{\"fact\": \"" + source.getFactPart + "\"}"))
        }
      }

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }

}
