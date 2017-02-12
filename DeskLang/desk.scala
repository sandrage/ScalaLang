import scala.util.parsing.combinator._

trait Merging{
	def merge(map: List[Map[String,Int]]):Map[String,Int]={
			map match{
				case List() => Map()
				case h::tl => h++merge(tl)
			}
	}
}
abstract class Values{
	def eval(variableValues: Map[String,Int]):Int
}

case class Number(value: Int) extends Values{
	def eval(variableValues: Map[String,Int]):Int={value}
}
case class Variable(name: String) extends Values{
	def eval(variableValues: Map[String,Int]):Int={variableValues.get(name).getOrElse(0) }
}
class DeskParser extends JavaTokenParsers with Merging{
	def desk = "print" ~> repsep(express,"+") ~ "where" ~ repsep(variables,",") ^^ {
		case listaValoriEVar ~ "where" ~ listaVariabili => {
			val mappaVars=merge(listaVariabili) 
			(sumAll(listaValoriEVar,mappaVars),mappaVars)
		}
	}
	def express = variable | constant
	def variables = """[a-zA-Z]""".r ~ "=" ~ wholeNumber ^^ { case variabile ~ "=" ~ valore => Map(variabile->valore.toInt)} 
	def variable = """[a-zA-Z]""".r ^^ { case variabile => new Variable(variabile)}
	def constant = """-?\d+""".r ^^ { case number => new Number(number.toInt)}

	def sumAll(valoriDaSommare: List[Values], mappaVariabili: Map[String,Int]):Int={
		valoriDaSommare match {
			case List() => 0
			case h::tl => (h.eval(mappaVariabili))+(sumAll(tl,mappaVariabili))
		}
	}
}

object DeskParserBuilder{
	def main(args: Array[String])={
		val parser=new DeskParser
		args.foreach{
			fileName => {
				val source=scala.io.Source.fromFile(fileName)
				val input=source.mkString
				parser.parseAll(parser.desk,input) match {
					case parser.Success((r,map),_) => println(r); println(map)
					case r => println("ERRORE: "+r)
				}
				source.close()
			}
		}
	}
}
