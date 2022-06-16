package fpinscala.exercises.state


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = 
    rng.nextInt match
      case (a, r) if a < 0 => (-(a+1), r)
      case d => d

  def double(rng: RNG): (Double, RNG) = 
    val (i1, r1) = nonNegativeInt(rng)
    val (i2, r2) = nonNegativeInt(r1)
    val divisor = Int.MaxValue.toDouble + 1
    ((i1.toDouble / divisor + i2.toDouble) / divisor, r2)

  def intDouble(rng: RNG): ((Int,Double), RNG) = 
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i,d), r2)

  def doubleInt(rng: RNG): ((Double,Int), RNG) = 
    map(intDouble)((a: (Int, Double)) => (a._2,a._1))(rng)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1,d2,d3), r2)

  def intsDirectly(count: Int)(rng: RNG): (List[Int], RNG) = 
    if count < 1 then (Nil, rng)
    else
      val (i, r) = rng.nextInt
      val (t, r2) = ints(count - 1)(r)
      (i :: t, r2)

  def doubleFromMap(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue.toDouble + 1))(rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => {
      val (a,r) = ra(rng)
      val (b,r1) = rb(r)
      (f(a,b), r1)
    }


  def sequenceWithoutMap[A](rs: List[Rand[A]]): Rand[List[A]] = 
    rng => {
      rs.foldRight[(List[A], RNG)]((Nil, rng))((ra, rl) => {
        val (a, r) = ra(rl._2)
        ( a :: rl._1, r)
      })
    }

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(Nil: List[A]))(map2(_,_)(_::_))

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = 
    sequence[Int](List.fill(count)(_.nextInt))(rng)

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = 
    rng => {
      val (a, r1) = r(rng)
      f(a)(r1)
    }

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] = 
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    flatMap(ra)(a => map(rb)(b => f(a,b)))
  

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for
        a <- run
        b <- sb
      yield f(a,b)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s1) = run(s)
        f(a)(s1)


  def unit[S,A](a: A): State[S,A] =
    s => (a, s)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def sequence[S,A](rs: List[State[S,A]]): State[S, List[A]] =
    rs.foldRight(unit(Nil: List[A]))((a,b)=>a.map2(b)(_::_))

  def get[S]: State[S,S] =
    s => (s,s)

  def set[S](s: S): State[S, Unit] =
    _ => ((), s)

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get
      _ <- set(f(s))
    yield ()

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = 
    val actions: List[Machine => Machine] = inputs map { 
      case Input.Coin => {
        case m @ Machine(l,a,b) if a < 1 || !l => m
        case Machine(_,a,b) => Machine(false, a, b+1)
      }
      case Input.Turn => {
        case m @ Machine(l,a,b) if a < 1 || l => m
        case Machine(_,a,b) => Machine(true, a-1, b)
      }
    }
    val states = State.sequence(actions map State.modify)
    for
      _ <- states
      s <- State.get
    yield (s.coins, s.candies)
   

