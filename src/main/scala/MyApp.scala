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
          IO.now(v)
      })
    }
  }

  sealed trait Case[+E]
  final case class CaseError[E](error: E) extends Case[E]
  final case class CaseTerminated[E](error: Throwable) extends Case[E]

  implicit class ZioRecover[E, A](val io: IO[E, A]) {

    /**
      * IO.sync { 5 / 0 }.redeem3 (
      *     { _: MyDomainException => IO.now(500) }
      *   , { case _: ArithmeticException => IO.now(1000) }
      *   , IO.now(_ * 2)
      * )
      */
    def redeem3[E2, B](err: E => IO[E2, B], term: Throwable => IO[E2, B], succ: A => IO[E2, B]): IO[E2, B] =
      io.run[E, A].flatMap {
        case ExitResult.Completed(value) =>
          succ(value)
        case ExitResult.Failed(error, _) =>
          err(error)
        case ExitResult.Terminated(e :: _) =>
          term(e)
      }

    /**
      * IO.sync { 5 / 0 }.redeem3 (
      *     { _: MyDomainException => 500 }
      *   , { case _: ArithmeticException => 1000 }
      *   , _ * 2
      * )
      */
    def redeem3Pure[E2, B](err: E => B, term: Throwable => B, succ: A => B): IO[E2, B] =
      redeem3(err.andThen(IO.now), term.andThen(IO.now), succ.andThen(IO.now))

    /**
      * IO.sync { 5 / 0 }.attempt3.flatMap {
      *   case Right(value) =>
      *     IO.now(value * 2)
      *   case Left(CaseError(_: MyDomainException)) =>
      *     IO.now(500)
      *   case Left(CaseTerminated(_: ArithmeticException)) =>
      *     IO.now(1000)
      * }
      */
    def attempt3: IO[Nothing, Either[Case[E], A]] =
      redeem3Pure(e => Left(CaseError(e)), t => Left(CaseTerminated(t)), Right(_))

    /**
      * IO.sync { 5 / 0 }.catchTerminated {
      *   case _: ArithmeticException =>
      *     IO.now(1000)
      * }
      */
    def catchTerminated(recoverWith: Throwable => IO[E, A]): IO[E, A] =
      redeem3(IO.fail, recoverWith, IO.now)

    /**
      * IO.sync { 5 / 0 }.catchTerminated {
      *   case _: ArithmeticException =>
      *     1000
      * }
      */
    def catchTerminatedPure(recoverWith: Throwable => A): IO[E, A] =
      catchTerminated(recoverWith.andThen(IO.now))

    /**
      * IO.sync { 5 / 0 }.catchBoth {
      *   case CaseError(_: MyDomainException) =>
      *     IO.now(500)
      *   case CaseTerminated(_: ArithmeticException) =>
      *     IO.now(1000)
      * }
      */
    def catchBoth[E2](recoverWith: Case[E] => IO[E2, A]): IO[E2, A] =
      redeem3[E2, A](e => recoverWith(CaseError(e)), t => recoverWith(CaseTerminated(t)), IO.now)

    /**
      * IO.sync { 5 / 0 }.catchBothPure {
      *   case CaseError(_: MyDomainException) =>
      *     500
      *   case CaseTerminated(_: ArithmeticException) =>
      *     1000
      * }
      */
    def catchBothPure(recoverWith: Case[E] => A): IO[E, A] =
      catchBoth(recoverWith.andThen(IO.now))
  }

  implicit class ZioExt[R](val io: IO[Throwable, R]) {
    implicit def trifunctor: Just[R] = {
      io.redeem[Left[Throwable, Nothing], R](t => IO.fail(Left(t)), v => IO.now(v))
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

