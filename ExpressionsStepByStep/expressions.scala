import scala.util.parsing.combinator._

abstract class Arithmetic
case class Number(value:Int) extends Arithmetic{
	override def toString():String=value.toString()
}
case class Sub(first:Arithmetic,second:Arithmetic) extends Arithmetic{
	override def toString():String = "("+first.toString()+"-"+second.toString()+")"
}
case class Sum(first:Arithmetic,second:Arithmetic) extends Arithmetic {
	override def toString():String = "("+first.toString()+"+"+second.toString()+")"
}
case class Mult(first:Arithmetic,second:Arithmetic) extends Arithmetic{
	override def toString():String = "("+first.toString()+"*"+second.toString()+")"
}
case class Div(first:Arithmetic, second:Arithmetic) extends Arithmetic{
	override def toString():String = "("+first.toString()+"/"+second.toString()+")"
}

trait Evaluation{
	def eval(expression:Arithmetic) :Arithmetic= expression match {
		case Number(a) => Number(a)
		case Sum(Number(a),Number(b)) => Number(a+b)
		case Sum(a,b) => Sum(eval(a),eval(b))
		case Sub(Number(a),Number(b)) => Number(a-b)
		case Sub(a,b) => Sub(eval(a),eval(b))
		case Mult(Number(a),Number(b)) => Number(a*b)
		case Mult(a,b) => Mult(eval(a),eval(b))
		case Div(Number(a),Number(b)) => Number((a/b).toInt)
		case Div(a,b) => Div(eval(a),eval(b)) 
	}

	def printWithEval(expression: Arithmetic) :Unit= {
		println(expression.toString())
		def printWithEval(expression: Arithmetic): Unit={
			expression match{
				case Number(a) => {}
				case _ => val newExpr=eval(expression);println(""+newExpr.toString()); printWithEval(newExpr)
			}
		}
		printWithEval(expression)
	} 
}
class ExpressionsParser extends JavaTokenParsers with Evaluation{
	def parser = expr ^^ { case expression => printWithEval(expression)}
	def expr = sum | sub | mult | div | number
	def sum :Parser[Arithmetic]= "("~>expr~"+"~expr<~")" ^^ {case first~"+"~second => new Sum(first,second)}
	def sub :Parser[Arithmetic]= "("~>expr~"-"~expr<~")" ^^ {case first~"-"~second => new Sub(first,second)}
	def mult :Parser[Arithmetic]= "("~>expr~"*"~expr<~")" ^^ {case first~"*"~second => new Mult(first,second)}
	def div :Parser[Arithmetic]= "("~>expr~"/"~expr<~")" ^^ {case first~"/"~second => new Div(first,second)}
	def number :Parser[Arithmetic]= decimalNumber ^^ { case numb => new Number(numb.toInt)}
}
object StepByStepEvaluator{
	def main(args: Array[String])={
		val parser=new ExpressionsParser

		args.foreach{
			expression=> {
				parser.parseAll(parser.parser,expression) match {
					case parser.Success(result,_) => println("\n \n")
					case r => println("ERRORE NEL PARSING: "+r)
				}		
			}
		}
	}
}