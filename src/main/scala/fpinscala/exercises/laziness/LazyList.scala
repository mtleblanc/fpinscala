package fpinscala.exercises.laziness
import LazyList.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() ::  t().toList

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case _ if n <= 0 => Empty
    case Empty => Empty
    case Cons(h, t) => Cons(h, () => t().take(n-1))

  def drop(n: Int): LazyList[A] = this match
    case _ if n <= 0 => this 
    case Empty => Empty
    case Cons(h, t) => t().drop(n-1)

  def takeWhile(p: A => Boolean): LazyList[A] = 
    foldRight[LazyList[A]](Empty)((a, acc) => if p(a) then Cons(() => a, () => acc) else Empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, acc) => p(a) && acc)

  def headOption: Option[A] = 
    foldRight[Option[A]](None)((a,_) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] =
    foldRight(empty[B])((a, acc) => cons(f(a), acc))

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(empty[A])((a,acc) => if f(a) then cons(a, acc) else acc)

  def append[A2>:A](as: => LazyList[A2]): LazyList[A2] =
    foldRight(as)(cons)

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty[B])((a,acc) => f(a).append(acc))



  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((this, n)) {
      case (Cons(h,_), 1) => Some(h(), (empty[A],0))
      case (Cons(h,t), n) if n > 1 => Some(h(), (t(), n-1))
      case _ => None
    }


  def takeWhileViaUnfold(p : A => Boolean): LazyList[A] = 
    unfold(this) {
      case Cons(h,t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipwith[B,C](bs: LazyList[B])(f: (A,B) => C) =
    unfold((this, bs)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zipAll[B](bs: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, bs)) {
      case (Empty, Empty) => None
      case (a, b) => Some((a.headOption, b.headOption), (a.drop(1), b.drop(1)))
    }

  def startsWith[B](s: LazyList[B]): Boolean = 
    zipAll(s).takeWhile((a,b) => b.isDefined).forAll((a,b) => a.equals(b))

  def tails: LazyList[LazyList[A]] =
    unfold(this) {
      case Empty => None
      case ls @ Cons(h, t) => Some(ls, t())
    }.append(LazyList(empty))

  def scanRight[B](base: B)(combine: (A, => B) => B): LazyList[B] =
    foldRight(LazyList(base))((a,bs) => bs match
      case Cons(h, t) => cons(combine(a, h()), bs))

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = cons(1, ones)

  def continually[A](a: A): LazyList[A] = 
    lazy val single: LazyList[A] = cons(a, single)
    single

  def from(n: Int): LazyList[Int] = cons(n, from(n+1))

  lazy val fibs: LazyList[Int] = 
    def go(a: Int, b:Int): LazyList[Int] =
      cons(a, go(b, a+b))
    go(0,1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = 
    f(state) match
      case Some(a, s) => cons(a, unfold(s)(f))
      case None => Empty


  lazy val fibsViaUnfold: LazyList[Int] = 
    unfold((0,1))((a,b) => Some((a, (b, a+b))))

  def fromViaUnfold(n: Int): LazyList[Int] = 
    unfold(n)(n=>Some(n, n+1))

  def continuallyViaUnfold[A](a: A): LazyList[A] = 
    unfold(())(_=>Some(a,()))

  lazy val onesViaUnfold: LazyList[Int] = continuallyViaUnfold(1)
