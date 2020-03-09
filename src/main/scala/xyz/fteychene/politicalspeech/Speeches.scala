package xyz.fteychene.politicalspeech

import cats.Applicative
import cats.effect.Sync
import cats.implicits._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.http4s.circe.{jsonEncoderOf, jsonOf}
import org.http4s.{EntityDecoder, EntityEncoder}

import scala.util.Random

trait Speeches[F[_]] {

  def get(): F[Speeches.Speech]

}

object SpeechesLines {

  val first: List[String] = "Mesdames, messieurs," ::
    "Je reste fondamentalement persuadé que" ::
    "Dès lors, sachez que je me battrai pour faire admettre que" ::
    "Par ailleurs c'est en toute connaissance de cause que je peux affirmer aujourd'hui que" ::
    "je tiens a ous dire ici ma détermination sans faille pour clamer haut et fort que" ::
    "J'ai depuis longtemps (ai-je besoin de vous le rappeler ?), défendu l'idée que" ::
    "Et c'est en toute conscience que je déclare avec conviction que" ::
    "Et ce n'est certainement pas vous, mes chers compatriotes, qui me contredirez si je vous dis que" ::
    Nil

  val second: List[String] = "la conjoncture actuelle" ::
    "la situation d'exclusion que certain d'entre vous connaissent" ::
    "l'acuité des problèmes de la vie quotidienne" ::
    "la volonté farouche de sortir notre pays de la crise" ::
    "l'effort prioritaire en faveur du statut précaire des exclus" ::
    "le particularisme dû à notre histoire unique" ::
    "l'aspiration plus que légitime de chacun au progrès social" ::
    "la nécessité de répondre à votre inquiétude journalière, que vous soyez jeune ou âgés," ::
    Nil

  val third: List[String] = "doit s'integrer à la finalisation globale" ::
    "oblige à la prise en compte encore plus effective" ::
    "interpelle le citoyen que je suis et nous oblige tous à aller de l'avant dans la voie" ::
    "a pour conséquence obligatoire l'urgente nécessité" ::
    "conforte mon désir incontestable d'aller dans le sens" ::
    "doit nous amener au choxi réellement impératif" ::
    "doit prendre en compte les préoccupations de la population de base dans l'élaboration" ::
    "entraîne une mission somme toute des plus exaltantes pour moi : l'élaboration" ::
    Nil

  val fourth: List[String] = "d'un processus allant vers plus d'égalité" ::
    "d'un avenir s'orientant vers plus de progres et plus de justice" ::
    "d'une restructuration dans laquelle chacun pourra enfin retrouver sa dignité" ::
    "dune valorisation sans concession de nos caractères spécifiques" ::
    "d'un plan correspondant véritablement aux éxigences légitimes de chacun" ::
    "de solutions rapides correspondant aux grands axes sociaux prioritaires" ::
    "d'un programme plus humain, plus fraternel et plus juste" ::
    "d'un projet porteur de véritables espoirs, notamment pour les plus démunis" ::
    Nil
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

    val texts: List[List[String]] = SpeechesLines.first :: SpeechesLines.second :: SpeechesLines.third :: SpeechesLines.fourth :: Nil

    def getRandomElement[A](seq: Seq[A])(implicit random: Random): A =
      seq(random.nextInt(seq.length))


    def generateSpeech(): String = texts.tail.map(Random.shuffle(_)).foldLeft(texts.head){ (acc, e) => acc.zip(e).map(el => el._1+ " "+ el._2) }.mkString(". ")

    def get: F[Speeches.Speech] =
      Speech(generateSpeech()).pure[F]
  }
}
