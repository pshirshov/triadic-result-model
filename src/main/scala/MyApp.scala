import scalaz.zio.{IO, _}

import scala.concurrent.duration.Duration
import scala.language.{higherKinds, implicitConversions}

trait WithResult {
  type Or[E, R]
  type Just[R]
}

trait ZioResult extends WithResult {
  type Or[E, V] = IO[Either[Throwable, E], V]
  type Just[V] = IO[Left[Throwable, Nothing], V]

  implicit class ZioEitherExt[L, R](val io: IO[Throwable, Either[L, R]]) {
    def trifunctor: Or[L, R] = {
      io.redeem[Either[Throwable, L], R](t => IO.fail(Left(t)), {
        case Left(v) =>
          IO.fail(Right(v))
        case Right(v) =>
          IO.point(v)
      })
    }
  }

  implicit class ZioExt[R](val io: IO[Throwable, R]) {
    implicit def trifunctor: Just[R] = {
      io.redeem[Left[Throwable, Nothing], R](t => IO.fail(Left(t)), v => IO.point(v))
    }
  }

  def choice[L, R](v: => Either[L, R]): Or[L, R] = {
    IO.syncThrowable(v).trifunctor
  }

  def just[R](v: => R): Just[R] = {
    IO.syncThrowable(v).trifunctor
  }
}

trait Service extends WithResult {
  sealed trait AnError

  object AnError {
    case class SomeError() extends AnError
  }

  def unary1(): Just[Long]
  def unary2(): Just[Long]

  def alternative1(): Or[AnError, Long]
  def alternative2(): Or[AnError, Long]
  def alternative3(): Or[AnError, Long]

}

class ServiceImpl extends Service with ZioResult {
  override def unary1(): Just[Long] = just {
    99
  }

  override def unary2(): Just[Long] = just {
    ???
  }

  override def alternative1(): Or[AnError, Long] = choice {
    Right(0L)
  }

  override def alternative2(): Or[AnError, Long] = choice {
    Left(AnError.SomeError())
  }

  override def alternative3(): Or[AnError, Long] = choice {
    ???
  }
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
      .attempt.map(_.fold(t =>{ println(t) ;1}, _ => 0))
      .map(ExitStatus.ExitNow)
  }

  def myAppLogic: IO[Either[Throwable, Service#AnError], Unit] = {
    val service = new ServiceImpl()
    for {
      u1 <- service.unary1()
      a1 <- service.alternative1()
//      a2 <- service.alternative2()
//      u2 <- service.unary2()
//      a3 <- service.alternative3()
    } yield {
      println(u1)
      println(a1)
      ()
    }
  }

  def main(args: Array[String]): Unit = {
    object io extends RTS
    import io._
    println(unsafeRunSync(run(args.toList)))
  }

}

