import scala.util.parsing.combinator._
class Task(name:String,operations: List[Operation]){
	def executeAll = operations.foreach { operation => operation.execute } ; println(name+" executeAll")
}
abstract class Operation {def execute}
case class Remove(fileName: String) extends Operation{ def execute = println("remove")}
case class Rename(oldFile: String, newFile: String) extends Operation{def execute = println("rename")}
case class Merge(file1: String, file2: String, file3: String) extends Operation{ def execute = println("merge")}
case class Backup(file1: String, file2: String) extends Operation{ def execute = println("backup")}

object LogLangEvaluator extends JavaTokenParsers{
	def document = rep(task) ^^ {case taskList => taskList.foreach {t => t.executeAll }}
	def task = "task" ~> ident ~ "{" ~ rep(operations) <~ "}" ^^ {
		case name ~ "{" ~ listaOperations => new Task(name, listaOperations)
	}
	def operations = remove | rename | merge | backup 
	def fileName = stringLiteral ^^ { s => s.substring(s.length()-1) }
	def remove :Parser[Operation] = "remove" ~> fileName ^^ {
		case name => new Remove(name)
	}
	def rename :Parser[Operation] = "rename" ~> fileName ~ fileName ^^ {
		case nameOld ~ nameNew => new Rename(nameOld,nameNew)
	}
	def merge :Parser[Operation] = "merge" ~> fileName ~ fileName ~ fileName ^^ {
		case file1 ~ file2 ~ file3 => new Merge(file1,file2,file3)
	}
	def backup :Parser[Operation] = "backup" ~> fileName ~ fileName ^^ {
		case file1 ~ file2 => new Backup(file1,file2)
	}
	def main (args: Array[String]) = {
		args.foreach {
			filename => {
				val input = scala.io.Source.fromFile(filename)
				val source = input.mkString
				parseAll(this.document,source) match {
					case this.Success(result,_) => println(result) 
					case r => println("ERRORE PARSING "+r) 
				}
			} 
		}
	}
}
