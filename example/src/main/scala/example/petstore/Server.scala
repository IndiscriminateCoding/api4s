package example.petstore

import api4s.outputs.{ Created, Ok }
import cats.effect.IO
import example.petstore.Model._
import shapeless.{ :+:, CNil, Inl, Inr }

class Server(storage: Storage[IO]) extends Api[IO] {
  def findPets(tags: List[String], limit: Option[Int]): IO[List[Pet]] =
    storage.findByTags(tags, limit)

  def addPet(pet: NewPet): IO[Ok[Pet] :+: Created[Pet] :+: CNil] =
    storage.addOrUpdate(pet) map { case (pet, added) =>
      if (added) Inr(Inl(Created(pet)))
      else Inl(Ok(pet))
    }

  def findPetById(id: Long): IO[Pet] = storage.findById(id)

  def deletePet(id: Long): IO[Unit] = storage.delete(id)
}
