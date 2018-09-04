package com.scalamill.meow.Monoid

import cats.Monoid
import cats.implicits._

object MonoidExample extends App {

  val intMonoid = Monoid[Int]
  val strMonoid = Monoid[String]
  val listMonoid = Monoid[List[Int]]

  assert(intMonoid.combine(1,3) == 4)
  assert(strMonoid.combine("Hello ", "World") == "Hello World")
  assert(listMonoid.combine(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))

  assert(intMonoid.combine(1, intMonoid.empty) == 1)
  assert(strMonoid.combine("Hello World", strMonoid.empty) == "Hello World")
  assert(listMonoid.combine(List(1, 2, 3), listMonoid.empty) == List(1, 2, 3))

   val transactions = List(
    Transaction(TransactionType.INVALID_OR_NO_TRANSACTION, 40.0), 
    Transaction(TransactionType.CREDIT, 200.0),
    Transaction(TransactionType.INVALID_OR_NO_TRANSACTION, 50.0),
    Transaction(TransactionType.DEBIT, 50.0),
    Transaction(TransactionType.CREDIT, 300.0),
    Transaction(TransactionType.DEBIT, 100.0),
    Transaction(TransactionType.INVALID_OR_NO_TRANSACTION, 25.0)
  )

  println(reportSevice.report(transactions))

}

object TransactionType extends Enumeration {
    type TRANSXN = Value
    val CREDIT = Value("Credit")
    val DEBIT = Value("Debit")
    val INVALID_OR_NO_TRANSACTION = Value("InvalidOrNoTransaction")

  }
case class Transaction(transactionType: TransactionType.TRANSXN, amount: Double)

object CombineAllCredit extends Monoid[Transaction] {

  override def empty = Transaction(TransactionType.INVALID_OR_NO_TRANSACTION, 0)  

   override def combine(a: Transaction, b: Transaction): Transaction = {
    if(b.transactionType == TransactionType.CREDIT) 
    {
      a.copy(transactionType = TransactionType.CREDIT, amount = a.amount + b.amount)
    } else {
      a
    }
  }
}


object CombineAllDebit extends Monoid[Transaction] {

  override def empty = Transaction(TransactionType.INVALID_OR_NO_TRANSACTION, 0)  

  override def combine(a: Transaction, b: Transaction): Transaction = {
    if(b.transactionType == TransactionType.DEBIT) 
    {
      a.copy(transactionType = TransactionType.DEBIT, amount = a.amount + b.amount)
    } else {
      a
    }
  }
}

object Finalbalance extends Monoid[Transaction] {

  override def empty = Transaction(TransactionType.INVALID_OR_NO_TRANSACTION, 0)  

  override def combine(a: Transaction, b: Transaction): Transaction = {
    if (b.transactionType == TransactionType.DEBIT) {
      a.copy(amount = a.amount - b.amount)
    }
    else if (b.transactionType == TransactionType.CREDIT) {
      a.copy(amount = a.amount + b.amount)
    } else {
      a
    }
  }
}




