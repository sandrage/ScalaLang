import java.io._
import scala.util.parsing.combinator._
trait ControlledFileManager{
	def control(file:File):Boolean={

		return (file.canRead() && file.exists() && file.isFile())
	}

	def writeFromInto(fromFile:File, toFile:File, append: Boolean) ={
		val buf: BufferedReader=new BufferedReader(new FileReader(fromFile))
		val bufOut: BufferedWriter= new BufferedWriter(new FileWriter(toFile,append))

		var line:String =buf.readLine()

		while(line!=null){
			bufOut.write(line,0,line.length)
			bufOut.newLine()
			line=buf.readLine()
		}
		bufOut.flush()
		bufOut.close()
		buf.close()
	}

}
abstract class Operation extends ControlledFileManager{
	def execute:Boolean
}
case class Remove(fileName: String) extends Operation with ControlledFileManager{
	def execute:Boolean={
		val file: File = new File(fileName)
		return (control(file) && file.delete())
	}
}
case class Rename(oldName: String, newName:String) extends Operation with ControlledFileManager{
	def execute:Boolean={
		val file: File=new File(oldName)
		val newFile: File=new File(newName)
		return (control(file) && file.renameTo(newFile))
	}
}
case class Backup(fromFile: String, toFile:String) extends Operation with ControlledFileManager{
	def execute: Boolean={
		val file:File=new File(fromFile)
		val newFile= new File(toFile)
		if(!newFile.createNewFile()) return false
		if( !control(file) ) return false
		writeFromInto(file,newFile,false)
		return true
	}
}
case class Merge(fromFirstFile:String, fromSecondFile:String, toFile:String) extends Operation with ControlledFileManager{

		def execute:Boolean={
			val firstFile=new File(fromFirstFile)
			val secondFile=new File(fromSecondFile)

			val newFile=new File(toFile)
			if (!control(firstFile) || !control(secondFile)) return false
			writeFromInto(firstFile,newFile,false)
			writeFromInto(secondFile,newFile,true)
			return true
		}
}

class Task(name: String, operations: List[Operation]){
	def executeAll():String={

		def executeAll(acc: Int, op: List[Operation]):String={
			op match {
				case List() => ""
				case first::tail => " [op"+acc+"]"+first.execute+"\n"+executeAll(acc+1,tail)
			}
		}
	"Task "+name+"\n"+executeAll(1,operations)
	}
}

class TaskParser extends JavaTokenParsers{
	def parser = rep(task) ^^ {case tasksList => {
		var output:String=""
		for{t<-tasksList} output+=t.executeAll()
		output
		}
	}
	def task = "task"~>ident~"{"~rep(tasks)<~"}" ^^ { case taskName~"{"~operations => new Task(taskName,operations)}
	def nome = stringLiteral ^^ { case name=> name.substring(1,name.length-1) }
	def tasks = remove | rename | backup | merge
	def remove:Parser[Operation]= "remove" ~>nome ^^ {case fileName=> new Remove(fileName)}
	def rename:Parser[Operation]= "rename"~>nome~nome ^^ { case oldFile~newFile => new Rename(oldFile,newFile)}
	def backup:Parser[Operation]= "backup"~>nome~nome ^^ { case fromFile~toFile => new Backup(fromFile,toFile)}
	def merge:Parser[Operation]= "merge"~>nome~nome~nome ^^ { case first~second~third => new Merge(first,second,third)}

}
object TaskParserBuilder {
	def main(args: Array[String])={
		args.foreach {
			fileName=>{
				val file=scala.io.Source.fromFile(fileName)
				val input=file.mkString
				val parser=new TaskParser
				parser.parseAll(parser.parser, input) match{
					case parser.Success(r,_) => println(r)
					case r => println("errore "+r)
				}
			}
		}
	}
}
