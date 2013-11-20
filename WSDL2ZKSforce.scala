import scala.xml._
import java.io._

class TypeInfo(xmlName: String, objcName: String, propertyFlags: String) {
	var xmlTypeName: String = xmlName
	var objcTypeName: String = objcName
	var objcPropFlags: String = propertyFlags
	
	def propertyDeclComment(): String =  { "" }
}

class ComplexTypeInfo(xmlName: String) extends TypeInfo(xmlName, "ZK" + xmlName.capitalize, "retain") {
}

class ArrayTypeInfo(ct: TypeInfo) extends TypeInfo(ct.xmlTypeName, "NSArray *", "retain") {
	val componentType = ct

	override def propertyDeclComment(): String =  { " // of " + componentType.objcTypeName }
}

class ComplexTypeProperty(name: String, propType: TypeInfo) {
	
	def readonlyPropertyDecl() :String = {
		"@property (" + propType.objcPropFlags + ") " + propType.objcTypeName + " " + name + ";" + propType.propertyDeclComment
	}
}

object WSDL2ZKSforce {
	def main(args: Array[String]) {
		val types = Map(
					"xsd:string" -> new TypeInfo("string", 	"NSString *", "retain"),
					   "xsd:int" -> new TypeInfo("int",    	"NSInteger",  "assign"),
					"xsd:boolean"-> new TypeInfo("boolean", "BOOL", 	  "assign"),
					"tns:ID"	 -> new TypeInfo("ID",		"NSString *", "retain")
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
		val xmlt = (field \ "@type").text
		val name = (field \ "@name").text
		val singleType = types.getOrElse(xmlt, new ComplexTypeInfo(name))
		val t = if (array) new ArrayTypeInfo(singleType) else singleType
		val prop = new ComplexTypeProperty(name, t)
		h.println(prop.readonlyPropertyDecl)
	}
}

WSDL2ZKSforce.main(args)
