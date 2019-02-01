import java.util.concurrent.atomic.AtomicReference

trait BankAccount {

  def closeAccount(): Unit

  def getBalance: Option[Int]

  def incrementBalance(increment: Int): Option[Int]
}

object Bank {
  type Balance = Int
  def openAccount(): BankAccount = new BankAccount {
    private val balanceRef: AtomicReference[Option[Balance]] =
      new AtomicReference[Option[Balance]](Some(0))

    override def closeAccount(): Unit = balanceRef.set(None)

    override def getBalance: Option[Int] = balanceRef.get()

    override def incrementBalance(increment: Int): Option[Int] = balanceRef.updateAndGet(
      _.map(_ + increment)
    )
  }
}
