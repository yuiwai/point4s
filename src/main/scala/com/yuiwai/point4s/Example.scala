package com.yuiwai.point4s

import cats.Id
import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.chaining._

object Example {
  type W = Wallet[Int, Int, Point[Int, Int]]

  def main(args: Array[String]): Unit = {
    core()
    // app()
  }

  def core(): Unit = {
    val wallet = Wallet.zero[Int, Int, Point[Int, Int]]
    wallet
      .addPoint(10, 0).tap(println)
      .addPoint(11, 1).tap(println)
  }

  def app(): Unit = {
    val repository = WalletRepositoryInmemoryImpl(Map.empty)
    val application = new WalletApplication[Id, Int, Int, Int, Point[Int, Int]](repository)
    val id = 100
    val now = 1234

    application
      .addPointTo(id, 1, 10, now)
      .tap(println)
      .foreach(wallet => assert(wallet.balance == 10))
    application
      .consumePointFrom(id, 1, 5, now)
      .tap(println)
      .tap(x => assert(x.isRight))
      .foreach(wallet => assert(wallet.balance == 5))
    application
      .consumePointFrom(id, 1, 10, now)
      .tap(println)
      .tap(x => assert(x.isLeft))
    application
      .addPointTo(id, 1, 5, now)
      .tap(println)
      .foreach(wallet => assert(wallet.balance == 10))
    application
      .consumePointFrom(id, 1, 10, now)
      .tap(x => assert(x.isRight))
      .foreach(wallet => assert(wallet.balance == 0))
  }

  final case class WalletRepositoryInmemoryImpl(private var data: Map[Int, Aggregation[Int, W]]) extends
    WalletRepository[Id, Int, Int, Int, Point[Int, Int]] {
    override def resolve(id: Int): Id[Option[Aggregation[Int, Root]]] = data.get(id)
    override def store(id: Int, aggregation: Aggregation[Int, Root]): Id[Either[RepositoryError, Int]] = {
      data = data.updated(id, aggregation) // TODO バージョンチェック
      Right(id)
    }
  }

  final case class WalletRepositoryAsyncInmemoryImpl(data: Map[Int, W]) extends
    WalletRepository[Future, Int, Int, Int, Point[Int, Int]] {
    override def resolve(id: Int): Future[Option[Aggregation[Int, Root]]] = ???
    override def store(id: Int, aggregation: Aggregation[Int, Root]): Future[Either[RepositoryError, Int]] = ???
  }
}

