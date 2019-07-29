package example.petstore

import cats.effect.IO
import example.petstore.Model._
import example.petstore.Server._

class Server extends Api[IO] {
  private[this] var lastId = 0L
  private[this] var pets: List[Pet] = Nil

  def findPets(tags: Option[String], limit: Option[Int]): IO[List[Pet]] = IO.delay(synchronized {
    println(s"findPets(tags = $tags, limit = $limit)")
    val matched = pets.filter(p => tags.isEmpty || tags == p.tag)

    limit.fold(matched)(lim => matched.take(lim))
  })

  def addPet(p: NewPet): IO[Pet] = IO.delay(synchronized {
    println(s"addPet(pet = $p)")
    val pet = Pet(id = lastId, name = p.name, tag = p.tag)
    lastId += 1
    pets = pet :: pets
    pet
  })

  def findPetById(id: Long): IO[Pet] = IO.suspend(synchronized {
    println(s"findPetById(id = $id)")
    pets.find(_.id == id) match {
      case None => IO.raiseError(new PetNotFound(id))
      case Some(p) => IO.pure(p)
    }
  })

  def deletePet(id: Long): IO[Unit] = IO.delay(synchronized {
    println(s"deletePet(id = $id)")
    pets = pets.filter(_.id == id)
  })
}

object Server {
  class PetNotFound(id: Long) extends Exception(s"Pet with id=$id not found!")
}
