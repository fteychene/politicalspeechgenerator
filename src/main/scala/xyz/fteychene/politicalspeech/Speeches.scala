package xyz.fteychene.politicalspeech

import cats.Applicative
import cats.implicits._
import cats.effect.Sync
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.http4s.Method.GET
import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.circe.{jsonEncoderOf, jsonOf}
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import xyz.fteychene.politicalspeech.Jokes.{Joke, JokeError}

import scala.util.Random

trait Speeches[F[_]] {

  def get(): F[Speeches.Speech]

}

object Speeches {

  implicit val random = new Random

  def apply[F[_]](implicit ev: Speeches[F]): Speeches[F] = ev

  case class Speech(speech: String)

  object Speech {
    implicit val speechDecoder: Decoder[Speech] = deriveDecoder[Speech]

    implicit def speechEntityDecoder[F[_] : Sync]: EntityDecoder[F, Speech] = jsonOf

    implicit val speechEncoder: Encoder[Speech] = deriveEncoder[Speech]

    implicit def speechEntityEncoder[F[_] : Applicative]: EntityEncoder[F, Speech] = jsonEncoderOf
  }

  final case class SpeechError(e: Throwable) extends RuntimeException

  def impl[F[_] : Sync](): Speeches[F] = new Speeches[F] {

    val texts: List[List[String]] =
      List("Mesdames, messieurs,", "Je reste fondamentalement persuadé que", "Dès lors, sachez que je me battrai pour faire admettre que") ::
        List("la conjoncture actuelle", "la situation d'exclusion que certain d'entre vous connaissent", "l'acuité des problèmes de la vie quotidienne") ::
        List("doit s'integrer à la finalisation globale", "oblige à la prise en compte encore plus effective", "interpelle le citoyen que je suis et nous oblige tous à aller de l'avant dans la voie") ::
        List("d'un processus allant vers plus d'égalité", "d'un avenir s'orientant vers plus de progres et plus de justice", "d'une restructuration dans laquelle chacun pourra enfin retrouver sa dignité") ::
        Nil

    def getRandomElement[A](seq: Seq[A])(implicit random: Random): A =
      seq(random.nextInt(seq.length))


    def generateSpeech(): String = texts.map(getRandomElement).mkString(" ")

    def get: F[Speeches.Speech] =
      Speech(generateSpeech()).pure[F]
  }
}
