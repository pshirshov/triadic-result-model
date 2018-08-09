import scalaz.zio.{IO, _}

import scala.concurrent.duration.Duration
import scala.language.{higherKinds, implicitConversions}

trait WithResult {
  type RAlt[E, R]
  type RUni[R]
}

object XIO {
  type XRAlt[E, V] = IO[Either[Throwable, E], V]
  type XRUni[V] = IO[Left[Throwable, Nothing], V]

  implicit def convert[L, R](io: IO[Throwable, Either[L, R]]): XRAlt[L, R] = {
    io.redeem[Either[Throwable, L], R](t => IO.fail(Left(t)), {
      case Left(v) =>
        IO.fail(Right(v))
      case Right(v) =>
        IO.point(v)
    })
  }

  implicit def convert1[R](io: IO[Throwable, R]): XRUni[R] = {
    io.redeem[Left[Throwable, Nothing], R](t => IO.fail(Left(t)), v => IO.point(v))
  }

  def apply[L, R](v: => Either[L, R]): XRAlt[L, R] = {
    IO.syncThrowable(v)
  }

  def unary[R](v: => R): XRUni[R] = {
    IO.syncThrowable(v)
  }
}

trait Service extends WithResult {
  sealed trait AnError

  object AnError {
    case class SomeError() extends AnError
  }

  def unary1(): RUni[Long]
  def unary2(): RUni[Long]

  def alternative1(): RAlt[AnError, Long]
  def alternative2(): RAlt[AnError, Long]
  def alternative3(): RAlt[AnError, Long]

}

class ServiceImpl extends Service {
  import XIO._
  type RAlt[E, V] = XRAlt[E, V]
  type RUni[V] = XRUni[V]

  override def unary1(): RUni[Long] = XIO.unary {
    99
  }

  override def unary2(): RUni[Long] = XIO.unary {
    ???
  }

  override def alternative1(): RAlt[AnError, Long] = XIO {
    Right(0L)
  }

  override def alternative2(): RAlt[AnError, Long] = XIO {
    Left(AnError.SomeError())
  }

  override def alternative3(): RAlt[AnError, Long] = XIO {
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

