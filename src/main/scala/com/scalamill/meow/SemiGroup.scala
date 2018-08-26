package com.scalamill.meow

import cats.Semigroup
import cats.implicits._
import com.scalamill.meow.SemiGroupExample.TransactionType

object SemiGroupExample extends App {

  val intSemiGroup = Semigroup[Int]
  val stringSemiGroup = Semigroup[String]
  val listSemiGroup = Semigroup[List[Int]]

  assert(intSemiGroup.combine(1, 3) == 4)
  assert(stringSemiGroup.combine("Hello", " World") == "Hello World")
  assert(listSemiGroup.combine(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))


  object TransactionType extends Enumeration {
    type TRANSXN = Value

    val Credit = Value("Credit")
    val Debit = Value("Debit")
  }

  val transactions = List(

    Transaction(TransactionType.Credit, 200.0),
    Transaction(TransactionType.Debit, 50.0),
    Transaction(TransactionType.Credit, 300.0),
    Transaction(TransactionType.Debit, 100.0)
  )

  println(reportSevice.report(transactions))

}

case class Transaction(transactionType: TransactionType.TRANSXN, amount: Double)

object CombineAllCredit extends Semigroup[Transaction] {
  override def combine(a: Transaction, b: Transaction): Transaction = {
    if (a.transactionType == TransactionType.Credit && b.transactionType == TransactionType.Credit) {
      a.copy(amount = a.amount + b.amount)
    } else {
      a
    }
  }
}

object CombineAllDebit extends Semigroup[Transaction] {
  override def combine(a: Transaction, b: Transaction): Transaction = {
    if (a.transactionType == TransactionType.Debit && b.transactionType == TransactionType.Debit) {
      a.copy(amount = a.amount + b.amount)
    } else {
      a
    }
  }
}

object Finalbalance extends Semigroup[Transaction] {
  override def combine(a: Transaction, b: Transaction): Transaction = {
    if (b.transactionType == TransactionType.Debit) {
      a.copy(amount = a.amount - b.amount)
    }
    else if (b.transactionType == TransactionType.Credit) {
      a.copy(amount = a.amount + b.amount)
    } else {
      a
    }
  }
}

object reportSevice {
  def report(transactions: Seq[Transaction]) = {
    val Transaction(_, totalCredit) = transactions.reduceLeft(CombineAllCredit.combine)
    val Transaction(_, totalDebit) = transactions.reduceLeft(CombineAllDebit.combine)
    val Transaction(_, finalbalance) = transactions.reduceLeft(Finalbalance.combine)
    s"Total Credit is $totalCredit and total debit is $totalDebit and final Balance is $finalbalance"
  }
}

