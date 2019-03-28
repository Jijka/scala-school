package java2scala.homeworks

trait CharAutomata[+A] {

  /** потребить один символ и перейти в следующее состояние */
  def consume(char: Char): CharAutomata[A]

  /** получить текущий результат, если это конец строки */
  def result: Either[String, A]

  /** потребить строку символ за символом */
  def apply(source: String): Either[String, A] =
    if (source.isEmpty) result
    else consume(source.head)(source.tail)

  /** создать автомат, который запустит оба автомата
    * и если первый вернёт ошибку, вернёт результат второго
    */
  def or[B](auto: CharAutomata[B]): CharAutomata[Either[A, B]] = new CharAutomata.Or(this, auto)

  /** создать автомат, который запустит оба автомата
    * вернёт результаты обоих, если оба вернут успешный результат
    * и вернёт ошибку, если вернёт ошибку хотя бы один
    */
  def and[B](auto: CharAutomata[B]): CharAutomata[(A, B)] = new CharAutomata.And(this, auto)
}

object CharAutomata {

  /** создаёт автомат, всегда результирующий с ошибкой
    * с заданным текстом message
    */
  def error(message: String): CharAutomata[Nothing] = new Error(message)

  /** создаёт автомат, всегда успешно результирующий
    * с заданным значением `value`
    */
  def const[A](value: A): CharAutomata[A] = new Const[A](value)

  /** создаёт автомат, возвращающий
    * первое вхождение строчки `substring`
    * или ошибкой
    */
  def find(substring: String): CharAutomata[Int] = new Find(substring)(FindResult(0, 0))

  /** создаёт автомат, определяющий является ли строчка,
    * если исключить из неё все символы, кроме `'('` и `')'`
    * корректной скобочной последовательностью */
  def balance: CharAutomata[Unit] = new ParenBalance(Balanced)

  /** создаёт автомат, ищущий первое число, из цифр подряд
    * и возвращающий результат в качестве BigInt либо 0
    */
  def parseInt: CharAutomata[BigInt] = new ParseInteger(IntegerHead)

  /** класс для реализации метода `error` */
  class Error(string: String) extends CharAutomata[Nothing] {
    def consume(char: Char): CharAutomata[Nothing] = this

    def result: Either[String, Nothing] = Left(string)
  }

  /** класс для реализации метода `const` */
  class Const[A] private[CharAutomata] (value: A) extends CharAutomata[A] {
    def consume(char: Char): CharAutomata[A] = this

    def result: Either[String, A] = Right(value)
  }

  sealed trait FindState
  case class FindResult(position: Int, state: Int) extends FindState

  /** класс для реализации метода `find` */
  class Find private[CharAutomata] (substring: String)(state: FindResult) extends CharAutomata[Int] {

    def stateTransitionFunction(state: Int, char: Char): Int =
      (Math.min(substring.length, state + 1) to 0 by -1)
        .find(i => (substring.substring(0, state) + char).endsWith(substring.substring(0, i)))
        .getOrElse(0)

    def consume(char: Char): CharAutomata[Int] = result match {
      case Right(_) => this
      case Left(_)  => new Find(substring)(FindResult(state.position + 1, stateTransitionFunction(state.state, char)))
    }

    def result: Either[String, Int] =
      if (substring.isEmpty) Right(0)
      else if (substring.length == state.state) Right(state.position - substring.length)
      else Left("Not Found")
  }

  /** класс для реализации метода `balance` */
  class ParenBalance private[CharAutomata] (balance: BalancedState) extends CharAutomata[Unit] {
    def consume(char: Char): CharAutomata[Unit] = char match {
      case '(' =>
        balance match {
          case Balanced        => new ParenBalance(Ambiguity())
          case Ambiguity(deep) => new ParenBalance(Ambiguity(deep + 1))
          case _               => this
        }
      case ')' =>
        balance match {
          case Balanced        => new ParenBalance(Imbalanced)
          case Ambiguity(0)    => new ParenBalance(Balanced)
          case Ambiguity(deep) => new ParenBalance(Ambiguity(deep - 1))
          case _               => this
        }
      case _ => this
    }

    def result: Either[String, Unit] = balance match {
      case Balanced => Right(())
      case _        => Left("Incorrect")
    }
  }

  /** класс для реализации метода `parseInt` */
  class ParseInteger private[CharAutomata] (state: IntegerState) extends CharAutomata[BigInt] {
    def consume(char: Char): CharAutomata[BigInt] =
      if (char.isDigit) state match {
        case IntegerHead        => new ParseInteger(IntegerNext(char.asDigit))
        case IntegerNext(value) => new ParseInteger(IntegerNext(value * 10 + char.asDigit))
        case _                  => this
      } else
        state match {
          case IntegerNext(r) if r == 0 => new ParseInteger(IntegerHead)
          case IntegerNext(r)           => new ParseInteger(IntegerResult(r))
          case _                        => this
        }

    def result: Either[String, BigInt] = state match {
      case IntegerResult(result) => Right(result)
      case IntegerNext(result)   => Right(result)
      case _                     => Right(0)
    }
  }

  /** класс для реализации метода `and` */
  class And[A, B] private[CharAutomata] (autoA: CharAutomata[A], autoB: CharAutomata[B]) extends CharAutomata[(A, B)] {
    def consume(char: Char): CharAutomata[(A, B)] = new And(autoA.consume(char), autoB.consume(char))

    def result: Either[String, (A, B)] = (autoA.result, autoB.result) match {
      case (Right(a), Right(b)) => Right(a, b)
      case _                    => Left("And Error")
    }
  }

  /** класс для реализации метода `or` */
  class Or[A, B] private[CharAutomata] (autoA: CharAutomata[A], autoB: CharAutomata[B]) extends CharAutomata[Either[A, B]] {
    def consume(char: Char): CharAutomata[Either[A, B]] = new Or(autoA.consume(char), autoB.consume(char))

    def result: Either[String, Either[A, B]] = (autoA.result, autoB.result) match {
      case (Left(_), Right(b)) => Right(Right(b))
      case (Right(a), _)       => Right(Left(a))
      case (Left(a), _)        => Left(a)
    }
  }
}

sealed trait BalancedState
case object Balanced                extends BalancedState
case object Imbalanced              extends BalancedState
case class Ambiguity(deep: Int = 0) extends BalancedState

sealed trait IntegerState
case object IntegerHead                      extends IntegerState
case object IntegerError                     extends IntegerState
case class IntegerNext(result: BigInt = 0)   extends IntegerState
case class IntegerResult(result: BigInt = 0) extends IntegerState
