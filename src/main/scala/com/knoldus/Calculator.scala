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
      //      Calculator.execute(this, operands).value match {
      //        case Some(value) => value.get
      //        case None => throw new CalculatorException
      //      }
      this.name match {
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
  }

  def calculate(operator: String, operands: Seq[Double]): Future[Seq[Double]] = {
    if (operator == "Find") {

      Future {
        operands.foldLeft(List.empty[Double]) {
          (res, elem) =>
            if ("!".validateAndExecute(Seq(elem)).head > "^".validateAndExecute(Seq(6, elem)).head) elem :: res
            else res
        }
      }
    } else if (operator == "3op") {
      execute("fibonacci", operands).map(x => "even".validateAndExecute(x)).map(x => "sum".validateAndExecute(x))
    } else if (operator == "(a+b)^2") {
      val firstCalculation = calculate("+", execute("^", Seq(operands.head, 2)), execute("^", Seq(operands.last, 2)))
      val secondCalculation = calculate("*", Seq(2.0), execute("*", operands))
      calculate("+", firstCalculation, secondCalculation)
    }
    else
      execute(operator, operands)
  }

  private def calculate(operator: String, leftCalculation: Future[Seq[Double]], rightCalculation: Future[Seq[Double]]): Future[Seq[Double]] = {
    val futureResult = leftCalculation.flatMap(x => rightCalculation.map(y => x ++ y))
    calculate(operator, futureResult)
  }

  private def calculate(operator: String, future: Future[Seq[Double]]): Future[Seq[Double]] = {
    future.map(x => operator.validateAndExecute(x))
  }

  private def calculate(operator: String, rightOperands: Seq[Double], futureCalculation: Future[Seq[Double]]): Future[Seq[Double]] = {
    val futureResult = futureCalculation.map(x => x ++ rightOperands)
    calculate(operator, futureResult)
  }

  def execute(operator: Operator, operands: Seq[Double]): Future[Seq[Double]] = Future {
    operator.validateAndExecute(operands)
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
  val a = Calculator.calculate("(a+b)^2", Seq(354, 978))

  Thread.sleep(2000)
  println(a)
}