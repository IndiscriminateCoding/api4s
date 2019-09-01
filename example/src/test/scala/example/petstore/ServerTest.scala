package example.petstore

import api4s.outputs.{ Created, Ok }
import cats.effect.{ ContextShift, IO }
import example.petstore.Model.NewPet
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.scalatest.{ FlatSpec, Matchers }
import shapeless._

import scala.concurrent.ExecutionContext.Implicits.global

class ServerTest extends FlatSpec with Matchers {
  implicit val contextShift: ContextShift[IO] = IO.contextShift(global)

  // Let's start our server just from Main class
  Main.run(Nil).start.unsafeRunSync()

  private[this] val blazeClient = BlazeClientBuilder[IO](global)
    .resource
    .allocated
    .unsafeRunSync()
    ._1

  private[this] val client = new Http4sClient[IO](
    blazeClient,
    authority = Some(Uri.Authority(
      host = Uri.RegName("localhost"),
      port = Some(8080)
    ))
  )

  it should "create, update, find and delete pets" in {
    // Create
    val np = NewPet(name = "Fluffy", tag = Some("tag"))
    val (pet, created) = client.addPet(np).unsafeRunSync() match {
      case Inl(Ok(pet)) => pet -> true
      case Inr(Inl(Created(pet))) => pet -> false
      case Inr(Inr(cnil)) => cnil.impossible
    }

    created shouldBe true
    pet.name shouldBe np.name
    pet.tag shouldBe np.tag

    // Update
    val newTag = Some("new-tag")
    val updatedPet = pet.copy(tag = newTag)
    client.addPet(NewPet(name = np.name, tag = newTag)).unsafeRunSync()

    // Find by ID
    val findPet = client.findPetById(pet.id).unsafeRunSync()
    findPet shouldBe updatedPet

    // Find by tag
    val findTag = client.findPets(newTag.toList).unsafeRunSync()
    findTag shouldBe List(updatedPet)

    // Delete
    client.deletePet(pet.id).unsafeRunSync()
    client.findPets(newTag.toList).unsafeRunSync() shouldBe Nil
  }
}
