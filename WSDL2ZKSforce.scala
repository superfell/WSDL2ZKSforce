import scala.xml._
import java.io._

class TypeInfo(val xmlName: String, val objcName: String, val propertyFlags: String) {
	
	def propertyDeclComment(): String =  { "" }
}

class ComplexTypeInfo(xmlName: String) extends TypeInfo(xmlName, "ZK" + xmlName.capitalize, "retain") {
}

class ArrayTypeInfo(val componentType: TypeInfo) extends TypeInfo(componentType.xmlName, "NSArray *", "retain") {

	override def propertyDeclComment(): String =  { " // of " + componentType.objcName }
}

class ComplexTypeProperty(val name: String, val propType: TypeInfo) {
	
	def readonlyPropertyDecl() :String = {
		"@property (" + propType.propertyFlags + ") " + propType.objcName + " " + name + ";" + propType.propertyDeclComment
	}
}

object WSDL2ZKSforce {
	def main(args: Array[String]) {
		val types = Map(
					"string" -> new TypeInfo("string", 	"NSString *", "retain"),
					"int" 	 -> new TypeInfo("int",    	"NSInteger",  "assign"),
					"boolean"-> new TypeInfo("boolean", "BOOL", 	  "assign"),
					"ID"	 -> new TypeInfo("ID",		"NSString *", "retain")
					)
					
		val wsdl = XML.loadFile("./partner.wsdl")
		val schemas = (wsdl \ "types" \ "schema" )
		for (schema <- schemas)
			if ((schema \ "@targetNamespace").text == "urn:partner.soap.sforce.com")
				for (ct <- (schema \ "complexType"))
					generateComplexType(ct, types)
  	}

	def generateComplexType(ct: Node, types: Map[String, TypeInfo]) {
		val xmlName:String = (ct \ "@name").text
		val objName:String = "ZK" + xmlName
		val t = new ComplexTypeInfo(xmlName)
		
		val hfile = new File(new File("output"), objName + ".h")
		hfile.getParentFile().mkdirs()
		val h = new PrintWriter(hfile)
		h.println(("""#import "zkDeserializer.h"
		|@interface """ + objName + """ : ZKXMLDeserializer {
		|}
		""").stripMargin('|'))
		for (field <- (ct \ "sequence" \ "element")) {
			generateField(h, field, types)
		}
		h.println("@end")
		h.close()
	}
	
	def generateField(h: PrintWriter, field: Node, types: Map[String, TypeInfo]) {
		val max = (field \ "@maxOccurs").text
		val array = (max != "" && max != "1")
		val xmlt = elementType(field)
		val name = (field \ "@name").text
		val singleType = types.getOrElse(xmlt, new ComplexTypeInfo(xmlt))
		val t = if (array) new ArrayTypeInfo(singleType) else singleType
		val prop = new ComplexTypeProperty(name, t)
		h.println(prop.readonlyPropertyDecl)
	}
	
	def elementType(e: Node): String = {
		val t = (e \ "@type").text
		val c = t.indexOf(':')
		if (c == -1) t else t.substring(c+1)
	}
}

WSDL2ZKSforce.main(args)
