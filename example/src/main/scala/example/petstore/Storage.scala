package example.petstore

import cats.effect.IO
import com.typesafe.scalalogging.Logger
import example.petstore.Model.{ NewPet, Pet }

import scala.util.control.NoStackTrace

trait Storage[F[_]] {
  def findById(id: Long): F[Pet]

  def findByTags(tags: List[String], limit: Option[Int]): F[List[Pet]]

  def addOrUpdate(p: NewPet): F[(Pet, Boolean)]

  def delete(id: Long): F[Unit]
}

object Storage {
  class PetNotFound(id: Long) extends Exception(s"Pet (id=$id) not found!") with NoStackTrace

  /* Simple in-memory pet storage */
  def apply(): Storage[IO] = new Storage[IO] {
    private[this] val logger = Logger("Storage")

    private[this] var lastId = 0L
    private[this] var pets: List[Pet] = Nil

    def findById(id: Long): IO[Pet] = IO.defer(synchronized {
      logger.info(s"findPetById(id = $id)")
      pets.find(_.id == id) match {
        case None => IO.raiseError(new PetNotFound(id))
        case Some(p) => IO.pure(p)
      }
    })

    def findByTags(tags: List[String], limit: Option[Int]): IO[List[Pet]] =
      IO.delay(synchronized {
        logger.debug(s"findPets(tags = $tags, limit = $limit)")
        val needed = tags.toSet
        val matched = pets.filter(_.tag.exists(needed))

        limit.fold(matched)(lim => matched.take(lim))
      })

    def addOrUpdate(p: NewPet): IO[(Pet, Boolean)] = IO.delay(synchronized {
      logger.debug(s"addOrUpdate(name = ${p.name}, tag = ${p.tag})")
      pets.find(_.name == p.name) match {
        case None =>
          val pet = Pet(id = lastId, name = p.name, tag = p.tag)
          lastId += 1
          pets = pet :: pets
          pet -> false
        case Some(pet) =>
          val updated = pet.copy(tag = p.tag)
          pets = updated :: pets.filter(_.id == pet.id)
          pet -> true
      }
    })

    def delete(id: Long): IO[Unit] = IO.defer(synchronized {
      logger.debug(s"delete (id = $id)")
      pets.find(_.id == id) match {
        case None => IO.raiseError(new PetNotFound(id))
        case Some(_) =>
          pets = pets.filter(_.id != id)
          IO.unit
      }
    })
  }
}
