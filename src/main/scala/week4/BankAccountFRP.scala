package week4
import rx.{Var, Rx}


class BankAccountFRP {
  val balance = Var(0)
  def deposit(amount: Int): Unit = {
    if (amount > 0) {
      val prevBalance = balance()
      balance() = prevBalance + amount
    }
  }

  def withdraw(amount: Int): Unit = {
    if (0 < amount && amount <= balance()) {
      balance() = balance() - amount
    } else {
      throw new Error("insufficient funds")
    }
  }
}
