import scala.util.parsing.combinator._

object HTMLTranslator extends JavaTokenParsers {
	override def skipWhitespace = false
	def document = repsep(line,'\n') ^^ {
		case listona => "<html>\n"+listona.mkString("\n")+"\n</html>"
	}
	def line :Parser[String] = head | endLine | ol | ul | paragraph | lineFormatted ^^ { _.mkString }
	def head :Parser[String] = ("###" | "##" | "#") ~ lineWithoutHead ^^ {
		case "###" ~ other => "<h3>"+other+"</h3>"
		case "##" ~ other => "<h2>"+other+"</h2>"
		case "#" ~ other => "<h1>"+other+"</h1>"
	}

	def ol :Parser[String] = rep1sep(oli,'\n') ^^ {
		case elementList => "<ol>\n"+elementList.mkString+"</ol>"
	}
	def oli :Parser[String] = """\d+""".r ~> ". " ~> lineFormatted ^^ {
		case strings => "<li>"+strings+"</li>\n"
	}
	def ul :Parser[String] = rep1sep(li,'\n') ^^ {
		case elementList => "<ul>\n"+elementList.mkString+"</ul>"
	}
	def li :Parser[String] = "* " ~> lineFormatted ^^ {
		case strings => "<li>"+strings+"</li>\n"
	}

	def lineFormatted :Parser[String] = rep(bolded | emed | coded | text) ^^ {_.mkString}
	def bolded :Parser[String] = "**" ~> lineFormatted <~"**" ^^ {case strings => "<strong>"+strings+"</strong>"}
	def emed :Parser[String] = "_" ~> lineFormatted <~ "_" ^^ {case strings => "<em>"+strings+"</em>"}
	def coded :Parser[String] = "'" ~> lineFormatted <~ "'" ^^{case strings => "<code>"+strings+"</code>"}
	def text :Parser[String] = """[^-'_*\n]""".r ^^ {_.toString()}
	def endLine :Parser[String] = "---" ^^ {_ => "</hr>"}
	def paragraph :Parser[String] = text ~ lineFormatted ^^ {case first ~ second => "<p>"+first+second+"</p>" }
	def lineWithoutHead :Parser[String] = rep(bolded | emed | coded | text ) ^^ {_.mkString}

	def main(args: Array[String]) ={
		args.foreach {
			fileName => {
				val input=scala.io.Source.fromFile(fileName)
				val source=input.mkString
				this.parseAll(this.document,source) match {
					case this.Success(result,_) => println(result)
					case r => println("errore nel parsing: "+r)
				}
			}
		}
	}
}
