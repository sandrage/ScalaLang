/*ArnoldC*/
import scala.util.parsing.combinator._
import scala.collection.mutable._
class ArnoldCParser(var map: Map[String,Int], var stack: Stack[Int]) extends JavaTokenParsers {
	def program = "IT'S" ~> "SHOW" ~> "TIME" ~> codeBody <~ "YOU" <~ "HAVE" <~ "BEEN" <~ "TERMINATED" ^^ {case _ => (map,stack)}
	def codeBody :Parser[Any] = rep(print | declaration | assigning | conditional | loopSt)
	def element = stringa | decimalNumber | variableName ^^ {
		case value => value
	}
	def stringa = stringLiteral ^^{
		case stringona => stringona.substring(1,stringona.length()-1)
	}
	def variableName = """[a-zA-Z0-9]+""".r ^^{
		case name => map.get(name).getOrElse(0).toString()
	}
	def print = "TALK" ~> "TO" ~> "THE" ~> "HAND" ~> element ^^ {case element => println(element.toString())}

	def declaration = "HEY" ~> "CHRISTMAS" ~> "TREE" ~> ident ~ "YOU" ~ "SET" ~ "US" ~ "UP" ~ decimalNumber ^^ {
		case variableName ~ "YOU" ~ "SET" ~ "US" ~ "UP" ~ number => {
			map+=Tuple2(variableName,number.toInt)
			()
		}
	}
	def assigning = "GET" ~> "TO" ~> "THE" ~> "CHOPPER" ~> ident ~ "HERE" ~ "IS" ~ "MY" ~ "INVITATION" ~ (decimalNumber | variableName) ~ (rep(operations)) <~ "ENOUGH" <~ "TALK" ^^{
		case vName ~ "HERE" ~ "IS" ~ "MY" ~ "INVITATION" ~ elem ~ listaOperations => {
			
			stack=stack.push(elem.toInt)
			listaOperations.foreach{(stringona) => this.parseAll(this.operation,stringona)}
			map.update(vName,stack.pop())
			()
		}
	}
	def block :Parser[String] = """(?s)\[.*?\]""".r ^^ { s => s.substring(1,s.length()-1)}
	
	def conditional = "BECAUSE"~> "I'M" ~>"GOING"~> "TO" ~>"SAY" ~>"PLEASE" ~>(decimalNumber | variableName) ~ block ~ "BULLSHIT" ~ block <~ "YOU" <~"HAVE"<~ "NO"<~ "RESPECT"<~ "FOR" <~"LOGIC" ^^{
		case value ~  ifTrueList ~ "BULLSHIT" ~ ifFalseList => {
			if (value.toInt != 0) {
					this.parseAll(this.codeBody,ifTrueList)
				
			} else{
					this.parseAll(this.codeBody,ifFalseList)
				
			}
			()
		}
	}
	def loopSt = "STICK" ~> "AROUND" ~> ident ~ block <~ "CHILL" ^^ {
		case vname ~ ops => {
			
			while(map.get(vname).getOrElse(0).toInt != 0){
				this.parseAll(this.codeBody,ops)
			}
		}
	}
	def operation = sum | sub | mul | div | uguale | orOp | majorThan | andOp
	def sumString :Parser[String] = "GET" ~ "UP" ~ (decimalNumber | variableName) ^^{case "GET" ~ "UP" ~ number => "GET UP "+number}
	def subString :Parser[String] = "GET" ~ "DOWN" ~ (decimalNumber | variableName) ^^{case "GET" ~ "DOWN" ~ number => "GET DOWN "+number}
	def mulString :Parser[String] = "YOU'RE" ~ "FIRED" ~ (decimalNumber | variableName) ^^{case "YOU'RE" ~ "FIRED" ~ number => "YOU'RE FIRED "+number}
	def divString :Parser[String] = "HE" ~ "HAD" ~"TO" ~ "SPLIT" ~ (decimalNumber | variableName) ^^{case "HE" ~ "HAD" ~"TO" ~ "SPLIT" ~ number => "HE HAD TO SPLIT "+number}
	def ugualeString = "YOU" ~ "ARE" ~ "NOT" ~ "YOU" ~ "YOU" ~ "ARE" ~ "ME" ~ (decimalNumber | variableName) ^^{
		case "YOU" ~ "ARE" ~ "NOT" ~ "YOU" ~ "YOU" ~ "ARE" ~ "ME" ~ value => "YOU ARE NOT YOU YOU ARE ME "+value
	}
	def majorThanString = "LET" ~ "OFF" ~ "SOME" ~ "STEAM" ~ "BENNET" ~ (decimalNumber | variableName) ^^{
		case "LET" ~ "OFF" ~ "SOME" ~ "STEAM" ~ "BENNET" ~ value => "LET OFF SOME STEAM BENNET "+value
	}
	def orOpString = "CONSIDER" ~ "THAT" ~ "A" ~ "DIVORCE" ~ (decimalNumber | variableName) ^^{
		case "CONSIDER" ~ "THAT" ~ "A" ~ "DIVORCE" ~ value => "CONSIDER THAT A DIVORCE "+value
	}
	def andOpString = "KNOCK" ~ "KNOCK"~ (decimalNumber | variableName) ^^{
		case "KNOCK" ~ "KNOCK"~ value => "KNOCK KNOCK "+value
	}
	def operations :Parser[String] = sumString | subString | mulString | divString | ugualeString | majorThanString | orOpString | andOpString
	def sum = "GET" ~> "UP" ~> (decimalNumber | variableName) ^^{
		case number => stack=stack.push(stack.pop()+number.toInt)
	}
	def sub = "GET" ~> "DOWN" ~> (decimalNumber | variableName) ^^{
		case number => stack=stack.push(stack.pop()-number.toInt)
	}
	def mul = "YOU'RE" ~> "FIRED" ~> (decimalNumber | variableName) ^^{
		case number => stack=stack.push(stack.pop()*number.toInt)
	}
	def div = "HE" ~> "HAD" ~>"TO" ~> "SPLIT" ~> (decimalNumber | variableName) ^^{
		case number => stack=stack.push(stack.pop()/number.toInt)
	}

	def uguale = "YOU" ~> "ARE" ~> "NOT" ~> "YOU" ~> "YOU" ~> "ARE" ~> "ME" ~> (decimalNumber | variableName) ^^{
		case value => stack=stack.push(if (stack.pop()==value.toInt) {1} else {0})

	}
	def majorThan = "LET" ~> "OFF" ~> "SOME" ~> "STEAM" ~> "BENNET" ~> (decimalNumber | variableName) ^^{
		case value => stack=stack.push(if ( stack.pop() > value.toInt) {1} else {0})
	}
	def orOp = "CONSIDER" ~> "THAT" ~> "A" ~> "DIVORCE" ~> (decimalNumber | variableName) ^^{
		case value => stack=stack.push(stack.pop()|value.toInt)
	}
	def andOp = "KNOCK" ~> "KNOCK"~> (decimalNumber | variableName) ^^{
		case value => stack=stack.push(stack.pop()&value.toInt) 
	}
}

object ArnoldCEvaluation {
	def main(args: Array[String])={
		val parser=new ArnoldCParser(Map(),new Stack[Int]())
		args.foreach{
			fileName => {
				val source=scala.io.Source.fromFile(fileName)
				val input=source.mkString
				parser.parseAll(parser.program,input) match {
					case parser.Success(result,_) => println("\n\n"+result)
					case r => println("ERRORE: "+r)
				}
			}
		}
	}
}