import javax.xml.crypto.dsig.XMLSignature.SignatureValue
import week4.{BankAccountFRP => BankAccount}
import rx.{Rx}

def consolidated(accts: List[BankAccount]): Rx[Int] = {
  Rx(accts.map(_.balance()).sum)
}

val a = new BankAccount()
val b = new BankAccount()
val c = consolidated(List(a, b))
c()
a.deposit(40)
c()
b deposit(30)
c()
val xchange = Rx(254)
val inDollar = Rx(c() * xchange())
inDollar()
b withdraw 10
inDollar()
