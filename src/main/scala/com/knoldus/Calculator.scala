package com.knoldus

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


trait Validator {
  def validate(operands: Seq[Double]): Boolean
}

trait Operator extends Validator {
  def name: String

  //validate and execute - implement this function in trait
  def validateAndExecute(operands: Seq[Double]): Seq[Double] = {
    if (this.validate(operands)) this.execute(operands) else throw new CalculatorException
  }

  protected def execute(operands: Seq[Double]): Seq[Double]

}

class CalculatorException extends Exception

object Calculator {
  implicit def convertStrToOperator(str: String) = new Operator {


    override def validate(operands: Seq[Double]): Boolean = {
      str match {
        case _ if str == "+" || str == "-" || str == "*" || str == "/" || str == "^" || str == "gcd" =>
          if (operands.size == 2) true else false
        case _ if str == "sqrt" || str == "!" || str == "fibonacci" => if (operands.size == 1) true else false
        case _ if str == "sum" || str == "odd" || str == "even" => if (operands.size >= 1) true else false
        case _ => throw new CalculatorException
      }
    }

    override def name: String = str

    override protected def execute(operands: Seq[Double]): Seq[Double] = {
      Calculator.execute(this, operands).value match {
        case Some(value) => value.get
        case None => throw new CalculatorException
      }
    }
  }

  def calculate(operator: String, operands: Seq[Double]): Future[Seq[Double]] = {
    if (operator.validate(operands)) execute(operator, operands) else throw new CalculatorException
  }

  def execute(operator: Operator, operands: Seq[Double]): Future[Seq[Double]] = Future {
    operator.name match {
      case "+" => Seq(operands.sum)
      case "-" => operands match {
        case first :: second :: Nil => Seq(first - second)
        case Nil => throw new CalculatorException
      }
      case "*" => Seq(operands.foldLeft(1.0)((res, elem) => elem * res))
      case "/" => operands match {
        case first :: second :: Nil => Seq(first / second)
        case Nil => throw new CalculatorException
      }
      case "^" => Seq(math.pow(operands.head, operands.last))
      case "sqrt" => Seq(math.sqrt(operands.head))
      case "!" => Seq(factorial(operands.head))
      case "sum" => Seq(operands.sum)
      case "odd" => operands.filter(x => x % 2 == 1)
      case "even" => operands.filter(x => x % 2 == 0)
      case "gcd" => Seq(gcd(operands.head, operands.last))
      case "fibonacci" => fibonacci(operands.head).reverse
    }

  }

  private def fibonacci(d: Double): Seq[Double] = {
    def helper(number: Int, list: List[Double]): List[Double] = {
      if (number > d) list else {
        helper(number + 1, fibAtPlace(number.toInt) :: list)
      }
    }

    helper(1, List(0))
  }

  private def fibAtPlace(n: Int): Int = n match {
    case 0 | 1 => n
    case _ => fibAtPlace(n - 1) + fibAtPlace(n - 2)
  }

  private def findmax(doubles: List[Double]) = {
    def helper(max: Double, list: List[Double]): List[Double] = {
      list match {
        case first :: rest if first > max => helper(first, rest)
        case first :: rest => helper(max, rest)
        case Nil => list
      }

    }

    helper(0.0, List.empty[Double])
  }

  private def findMultiple(d: Double): Seq[Double] = {
    def helper(divisor: Int, multiples: List[Double]): List[Double] = {
      if (d < divisor) multiples else {
        if (d > divisor && d % divisor == 0) helper(divisor + 1, divisor :: multiples)
        else helper(divisor + 1, multiples)
      }
    }

    helper(1, List.empty[Double])
  }

  private def factorial(x: Double) = {
    def helper(x: Int, prod: Int): Double = {
      if (x > 0) {
        helper(x - 1, prod * x)
      } else prod
    }

    helper(x.toInt, 1)
  }

  private def gcd(first: Double, second: Double): Double =
    if (second == 0) first else gcd(second, first % second)
}

object Main extends App {
  val a = Calculator.calculate("gcd", Seq(12, 90))
  Thread.sleep(2000)
  println(a)
}