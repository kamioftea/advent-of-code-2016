import com.smartystreets.api.StaticCredentials
import com.smartystreets.api.us_street.{Batch, Client, Lookup}
import com.smartystreets.api.ClientBuilder

import scala.io.Source

object SmartyStreets extends App {

  val credentials = new StaticCredentials(System.getenv("SMARTY_AUTH_ID"), System.getenv("SMARTY_AUTH_TOKEN"))
  val client: Client = new ClientBuilder(credentials).buildUsStreetApiClient()
  val batch = new Batch

  Source
    .fromResource("mission-addresses.txt")
    .getLines()
    .map(new Lookup(_))
    .foreach(batch.add)

  client.send(batch)

  val valid: Long =
    batch.getAllLookups
      .stream()
      .filter(l => !l.getResult.isEmpty)
      .count()

  println(valid)
}


