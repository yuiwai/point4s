package com.yuiwai.point4s

import cats.Monad
import cats.implicits._

import scala.collection.SortedSet

// Core
final case class Wallet[A, T: Ordering, P <: Point[A, T] : Ordering]
(points: SortedSet[P], counter: Int)(implicit N: Numeric[A]) {
  lazy val balance: A = points.foldLeft(N.zero) { case (acc, p) => N.plus(acc, p.amount) }
  def addPoint(amount: A, now: T): Wallet[A, T, P] =
    copy(points = points.union(Set(Point(amount, counter, now).asInstanceOf[P])), counter = counter + 1)
  def consumePoint(amount: A, now: T): Either[WalletError, Wallet[A, T, P]] = {
    if (N.lteq(amount, N.zero)) Left(ConsumeNegativeOrZeroPoint)
    else consumePointImpl(amount, points).map(Wallet(_, counter))
  }
  @scala.annotation.tailrec
  private def consumePointImpl(amount: A, points: SortedSet[P]): Either[WalletError, SortedSet[P]] = {
    points.headOption match {
      case Some(p) if N.gteq(p.amount, amount) =>
        Right(points.tail.union(Set(p.copy(N.minus(p.amount, amount)).asInstanceOf[P])))
      case Some(p) => consumePointImpl(N.minus(amount, p.amount), points.tail)
      case None => Left(PointLacked)
    }
  }
}
object Wallet {
  def zero[A: Numeric, T: Ordering, P <: Point[A, T] : Ordering]: Wallet[A, T, P] = apply(SortedSet.empty[P], 1)
}

sealed trait WalletError
case object PointLacked extends WalletError
case object ConsumeNegativeOrZeroPoint extends WalletError

final case class Point[A: Numeric, T: Ordering](amount: A, index: Int, createdAt: T)
object Point {
    implicit def pointOrdering[A: Numeric, T: Ordering]: Ordering[Point[A, T]] =
      (x, y) => implicitly[Ordering[Int]].compare(x.index, y.index)
}

abstract class ExpirationRule[T: Ordering] {
  def apply(now: T): Boolean
}

// Application
final case class Aggregation[I, R](id: I, originVersion: Int, currentVersion: Int, root: R) {
  def modify(f: R => R): Aggregation[I, R] = copy(currentVersion = currentVersion + 1, root = f(root))
  def modifyE[E](f: R => Either[E, R]): Either[E, Aggregation[I, R]] =
    f(root).map(r => copy(currentVersion = currentVersion + 1, root = r))
}

sealed trait RepositoryError
case object EntityNotFound extends RepositoryError
case object VersionMismatched extends RepositoryError

trait Repository[F[_], I, R] {
  def resolve(id: I): F[Option[Aggregation[I, R]]]
  def resolve(id: I, version: Int): F[Either[RepositoryError, Aggregation[I, R]]] = ???
  def store(id: I, aggregation: Aggregation[I, R]): F[Either[RepositoryError, I]]
}

abstract class WalletRepository[F[_] : Monad, I, A: Numeric, T: Ordering, P <: Point[A, T] : Ordering]
  extends Repository[F, I, Wallet[A, T, P]] {
  type Root = Wallet[A, T, P]
}

sealed trait ApplicationError
object ApplicationError {
  def apply(walletError: WalletError): ApplicationError =
    walletError match {
      case PointLacked => WalletPointLacked
      case ConsumeNegativeOrZeroPoint => InvalidInput
    }
}
case object WalletNotFound extends ApplicationError
case object WalletPointLacked extends ApplicationError
case object InvalidInput extends ApplicationError

class WalletApplication[F[_] : Monad, I, A: Numeric, T: Ordering, P <: Point[A, T] : Ordering]
(walletRepository: WalletRepository[F, I, A, T, P]) {
  type Result[R] = F[Either[ApplicationError, R]]
  type W = Wallet[A, T, P]
  def addPointTo(id: I, version: Int, amount: A, now: T): Result[W] =
    for {
      loaded <- walletRepository.resolve(id)
      modified = loaded.orElse(Aggregation(id, 1, 1, Wallet.zero: W).some)
        .map(_.modify(_.addPoint(amount, now)))
      _ <- modified.traverse(walletRepository.store(id, _))
    } yield modified match {
      case Some(a) => Right(a.root)
      case None => Left(WalletNotFound)
    }
  def consumePointFrom(id: I, version: Int, amount: A, now: T): Result[W] =
    for {
      loaded <- walletRepository.resolve(id)
      modified = loaded.map(_.modifyE(_.consumePoint(amount, now)))
      _ <- modified.traverse(_.traverse(walletRepository.store(id, _)))
    } yield modified match {
      case Some(e) => e.fold[Either[ApplicationError, W]](ApplicationError(_).asLeft, _.root.asRight)
      case None => Left(WalletNotFound)
    }
}

// Service
final case class WalletSummary(version: Int)

final case class AddRequest[I, A, T](id: I, version: Int, amount: A, now: T)
final case class ConsumeRequest[I, A, T](id: I, version: Int, amount: A, now: T)

sealed trait ServiceError

trait PointService[I, A, T] {
  type Result = Either[ServiceError, WalletSummary]
  def add(addRequest: AddRequest[I, A, T]): Result
  def consume(consumeRequest: ConsumeRequest[I, A, T]): Result
  def summarize(): Result
  def normalize(): Result
}

