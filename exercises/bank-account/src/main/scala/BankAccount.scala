/*
   According to the book Scala in Depth [1],
   we do not need to use synchronized for balance lookup

   [1] Fragment on pages 16 and 17 of https://www.comcol.nl/code/inkijkexemplaar/9781935182702/scala-in-depth-engels-josh-suereth.pdf
 */

trait BankAccount {

  def closeAccount(): Unit
  def getBalance: Option[Int]
  def incrementBalance(increment: Int): Option[Int]
}

protected case class Account(var balance: Option[Int] = Some(0)) extends BankAccount {

  private def runThreadSafe[A](block: => A): A = this.synchronized(block)

  override def closeAccount(): Unit = runThreadSafe { balance = None }

  override def getBalance: Option[Int] = balance

  override def incrementBalance(increment: Int): Option[Int] = runThreadSafe {
    balance flatMap { amount =>
      balance = Some(amount + increment)
      balance
    }
  }
}

object Bank {
  def openAccount(): BankAccount = Account()
}
