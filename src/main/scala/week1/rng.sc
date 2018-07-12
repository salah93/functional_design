import week1._


val integers = new Generator[Int] {
  val rand = new java.util.Random()
  def generate: Int = rand.nextInt()
}
integers.generate
integers.generate
integers.generate
integers.generate

def booleans =  for {
    i <- integers
  } yield i > 0

booleans.generate
booleans.generate
booleans.generate
booleans.generate

def pairs[T, U](t: Generator[T],
                u: Generator[U]) =
  for {
  x <- t
  y <- u
} yield (x, y)

pairs(integers, integers).generate
pairs(integers, integers).generate
pairs(booleans, booleans).generate
pairs(booleans, integers).generate
pairs(integers, booleans).generate

def single[T](x: T): Generator[T] = new Generator[T] {
  override def generate: T = x
}

def choose(lo: Int, hi: Int): Generator[Int] = {
  for {
    x <- integers
  } yield lo + math.abs(x % (hi - lo))
}

def oneOf[T](xs: T*): Generator[T] = for {
  idx <- choose(0, xs.length)
} yield xs(idx)

val o = oneOf(1, 2, 3, 4)
o.generate
o.generate
o.generate


def lists: Generator[List[Int]] = for {
  isEmpty <- booleans
  list <- if (isEmpty) emptyLists else nonEmptyLists
} yield list

def emptyLists = single(Nil)

def nonEmptyLists = for {
  head <- integers
  tail <- lists
} yield head :: tail

lists.generate



trait Tree
case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree


def trees: Generator[Tree] = for {
  isLeaf <- booleans
  tree <- if (isLeaf) leafNode else branchTree
} yield tree

def leafNode: Generator[Leaf] = for {
  x <- integers
} yield Leaf(x)

def branchTree = for {
  left <- trees
  right <- trees
} yield Inner(left, right)


trees.generate

def test[T](r: Generator[T], noTimes: Int=100)(test: T => Boolean): Unit = {
  for (_ <- 0 until noTimes) {
    val value = r.generate
    assert(test(value), "test failed for: " + value)
  }
  println("Test passed " + noTimes + " times")
}

test(pairs(lists, lists)) {
  case (xs, ys) => (xs ++ ys).length > xs.length
}
