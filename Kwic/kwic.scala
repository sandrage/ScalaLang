import java.io._
trait ReadFrom{
	def read(r:Reader):List[String]
}
trait ReadFromFile extends ReadFrom{
	def read(r:Reader):List[String]={
		val bufferedReader=new BufferedReader(r)
		var line:String=bufferedReader.readLine()
		var lines:Set[String]=Set()
		while(line!=null){
			lines=lines+line.trim()
			line=bufferedReader.readLine()
		}
		lines.toList
	}
}
trait FormattingLines{
	def trimmingIntoTitle(s:String)={
		s.split("[\\s]").toList.map(str=>str.trim()).filter(str=>str!="").mkString(" ")
	}
}
case class KwicLine(lineNumber: Int, beforeKeyword:String, afterKeyword:String){
	def <=(line:KwicLine):Boolean=this.afterKeyword.toLowerCase()<=(line.afterKeyword.toLowerCase())
}

class Kwic(val source: String) extends ReadFromFile with FormattingLines{
	val index:List[KwicLine]=createIndex

	def createLines(number:Int,title:String)={
		val titleWords=title.split("[\\s]").toList.map(str=>str.trim())
		val perOgniRiga=for(i<-0 to (titleWords.length-1) if(titleWords(i).length>=2 
			&& !titleWords(i).toLowerCase().equals("and") 
			&& !titleWords(i).toLowerCase().equals("the")))
			 yield (new KwicLine(number,titleWords.slice(0,i).mkString(" "),titleWords.slice(i,titleWords.length).mkString(" ")))
		perOgniRiga.toList
	}
	def orderIndex(indexToReorder:List[KwicLine])={
		indexToReorder.sortWith((lineF,lineB)=>lineF<=lineB)
	}
	def createIndex={
		var titles=read(new FileReader(new File(source))).map(title=>trimmingIntoTitle(title))
		orderIndex((for(i<-1 to titles.length) yield createLines(i,titles(i-1))).toList.flatten)
	}

	override def toString()={
		var formatting=new StringBuilder("%5d %34s %s \n")
		var toOutput:String=""
		for(line<-createIndex){
			toOutput=toOutput+formatting.format(line.lineNumber,line.beforeKeyword,line.afterKeyword)
		}
		toOutput
	}

}
