import cats.data.EitherT
import scalaz.zio.{IO, _}
import scalaz.zio.interop._

import scala.concurrent.duration.Duration
import scala.language.higherKinds

object Instances {
  type Container[V] = IO[Throwable, V]

  implicit object f extends cats.Applicative[Container] {
    override def map[A, B](fa: Container[A])(f: A => B): Container[B] = fa.map(f)

    override def pure[A](x: A): Container[A] = IO.point(x)

    override def ap[A, B](ff: Container[A => B])(fa: Container[A]): Container[B] = ff.flatMap {
      fab =>
        fa.map {
          a =>
            fab(a)
        }
    }
  }
}

trait Service {
  type Container[V]
  type RAlt[E, R]
  type RUni[R]

  sealed trait AnError
  object AnError {
    case class SomeError() extends AnError
  }

  def throwing(): RAlt[AnError, Long]
  def unary(): RUni[Long]
}

class ServiceImpl extends Service {
  type Container[V] = IO[Throwable, V]
  type RAlt[E, R] = EitherT[Container, E, R]
  type RUni[R] = Container[R]

  import Instances._

  override def throwing(): RAlt[AnError, Long] = EitherT.fromEither(Right(0L))

  override def unary(): RUni[Long] = IO.point(99)
}

object MyApp {

  sealed trait ExitStatus

  object ExitStatus {

    case class ExitNow(code: Int) extends ExitStatus

    case class ExitWhenDone(code: Int, timeout: Duration) extends ExitStatus

    case object DoNotExit extends ExitStatus

  }

  def run(args: List[String]): IO[Void, MyApp.ExitStatus] = {
    myAppLogic
      .attempt.map(_.fold(_ => 1, _ => 0))
      .map(ExitStatus.ExitNow)
  }



  def myAppLogic: IO[Throwable, Unit] = {
    val service = new ServiceImpl()
    for {
      _ <- service.unary()
    } yield ()
  }



  def main(args: Array[String]): Unit = {
    object io extends RTS
    import io._
    println(unsafeRunSync(run(args.toList)))
  }

}

