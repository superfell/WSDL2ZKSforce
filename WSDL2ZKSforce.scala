import scala.xml._
import java.io._

class TypeInfo(xmlName: String, objcName: String, propertyFlags: String) {
	var xmlTypeName: String = xmlName
	var objcTypeName: String = objcName
	var objcPropFlags: String = propertyFlags
}

object WSDL2ZKSforce {
	def main(args: Array[String]) {
		val types = Map(
					"string" -> new TypeInfo("string", "NSString *", "retain"),
					   "int" -> new TypeInfo("int",    "NSInteger",  "assign")
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
		val t = new TypeInfo(xmlName, objName, "retain")
		
		val h = new PrintWriter(new File(objName + ".h"))
		h.print(("""#import "zkDeserializer.h"
		|@interface """ + objName + """ : ZKXMLDeserializer {
		|}
		|
		|@end
		|""").stripMargin('|'))
		h.close()
	}
}

WSDL2ZKSforce.main(args)
