import scala.xml._
import java.io._

class TypeInfo(val xmlName: String, val objcName: String, val propertyFlags: String) {
	
	def propertyDeclComment(): String =  { "" }
}

class ArrayTypeInfo(val componentType: TypeInfo) extends TypeInfo(componentType.xmlName, "NSArray *", "retain") {

	override def propertyDeclComment(): String =  { " // of " + componentType.objcName }
}

class ComplexTypeProperty(val name: String, val propType: TypeInfo) {
	
	def readonlyPropertyDecl() :String = {
		"@property (" + propType.propertyFlags + ") " + propType.objcName + " " + name + ";" + propType.propertyDeclComment
	}
}

object Direction extends Enumeration {
	val Serialize, Deserialize = Value
}

class ComplexTypeInfo(xmlName: String, fields: Seq[ComplexTypeProperty]) extends TypeInfo(xmlName, "ZK" + xmlName.capitalize, "retain") {
	
	val direction =  Direction.ValueSet.newBuilder
		
	def writeHeaderFile() {
		val hfile = new File(new File("output"), objcName + ".h")
		hfile.getParentFile().mkdirs()
		val h = new PrintWriter(hfile)
		h.println(("""#import "zkDeserializer.h"
					|@interface """ + objcName + """ : ZKXMLDeserializer {
					|}
					""").stripMargin('|'))
		for (f <- fields)
			h.println(f.readonlyPropertyDecl)
		h.println("@end")
		h.close()
	}
}


class Schema(wsdl: Elem, typeMapping: Map[String, TypeInfo]) {
	private val complexTypeElems = createComplexTypesElems(wsdl)
	private val complexTypes = collection.mutable.Map[String, ComplexTypeInfo]()
	private val elements = createElements(wsdl)
	private val simpleTypes = createSimpleTypes(wsdl)
	
	val messages = createMessages(wsdl)

	def handleInputElement(elementName: String) {
		handleElement(elementName, Direction.Serialize)
	}
	
	def handleOutputElement(elementName: String) {
		handleElement(elementName, Direction.Deserialize)
	}
	
	private def handleElement(elementName: String, dir: Direction.Value) {
		// walk through the element decl and build the related types.
		val element = elements(elementName)
		(element \ "complexType" \ "sequence" \ "element").map(generateField(_, dir))
	}
	
	def complexType(xmlName: String, dir: Direction.Value) : ComplexTypeInfo = {
		val t = complexTypes.getOrElse(xmlName, makeComplexType(xmlName, dir))
		t.direction += dir
		return t
	}
	
	def writeTypes() {
		for ((_, ct) <- complexTypes)
			ct.writeHeaderFile()
	}

	private def createSimpleTypes(wsdl: Elem): Map[String, Node] = {
		val schemas = (wsdl \ "types" \ "schema" )
		for (schema <- schemas) {
			if ((schema \ "@targetNamespace").text == "urn:partner.soap.sforce.com") {
				return (schema \ "simpleType").map( x => ( (x \ "@name").text, x )).toMap
			}
		}
		return collection.immutable.Map[String, Node]()
	}
	
	private def createElements(wsdl: Elem): Map[String, Node] = {
		val schemas = (wsdl \ "types" \ "schema" )
		for (schema <- schemas) {
			if ((schema \ "@targetNamespace").text == "urn:partner.soap.sforce.com") {
				return (schema \ "element").map( x => ( (x \ "@name").text, x )).toMap
			}
		}
		return collection.immutable.Map[String, Node]()
	}
	
	private def createMessages(wsdl: Elem): Map[String, Node] = {
		(wsdl \ "message").map( x => ((x \ "@name").text, x)).toMap
	}
	
	private def createComplexTypesElems(wsdl: Elem): Map[String, Node] = {
		val schemas = (wsdl \ "types" \ "schema" )
		for (schema <- schemas) {
			if ((schema \ "@targetNamespace").text == "urn:partner.soap.sforce.com") {
				return (schema \ "complexType").map( x => ( (x \ "@name").text, x )).toMap
			}
		}
		return collection.immutable.Map[String, Node]()
	}
	
	private def makeComplexType(xmlName: String, dir: Direction.Value): ComplexTypeInfo = {
		val ct = complexTypeElems(xmlName)
		// we insert a temporary version of the complexType to handle recursive definitions
		complexTypes(xmlName) = new ComplexTypeInfo(xmlName, List())
		val fields = (ct \ "sequence" \ "element").map( x => generateField(x, dir) )
		val i = new ComplexTypeInfo(xmlName, fields)
		i.direction += dir
		complexTypes(xmlName) = i
		return i
	}

	private def generateField(field: Node, dir: Direction.Value): ComplexTypeProperty = {
		val max = (field \ "@maxOccurs").text
		val array = (max != "" && max != "1")
		val xmlt = elementType(field)
		val name = (field \ "@name").text
		val singleType = getType(xmlt, dir)
		val t = if (array) new ArrayTypeInfo(singleType) else singleType
		new ComplexTypeProperty(name, t)
	}

	private def getType(xmlName: String, dir: Direction.Value): TypeInfo = {
		if (typeMapping contains xmlName) return typeMapping(xmlName)
		if (complexTypes contains xmlName) return complexType(xmlName, dir)
		if (simpleTypes contains xmlName) return typeMapping("string")	// are all simple types string extentions/restrictions ?
		makeComplexType(xmlName, dir)	// no where else, assume its a complexType we haven't processed yet
	}
	
	private def elementType(e: Node): String = {
		stripPrefix((e \ "@type").text)
	}
}

def stripPrefix(v: String): String = {
	val c = v.indexOf(':')
	if (c == -1) v else v.substring(c+1)
}

object WSDL2ZKSforce {
	def main(args: Array[String]) {
		val types = Map(
					"string" 		-> new TypeInfo("string", 		"NSString *", 	"retain"),
					"int" 	 		-> new TypeInfo("int",    		"NSInteger",  	"assign"),
					"boolean"		-> new TypeInfo("boolean", 		"BOOL", 	  	"assign"),
					"ID"	 		-> new TypeInfo("ID",			"NSString *", 	"retain"),
					"sObject"		-> new TypeInfo("sObject", 		"ZKSObject *",	"retain"),
					"dateTime"		-> new TypeInfo("dateTime",		"NSDate *",  	"retain"),
					"date"   		-> new TypeInfo("date",    		"NSDate *",   	"retain"),
					"base64Binary" 	-> new TypeInfo("base64Binary", "NSData *", 	"retain")
					)
					
		val wsdl = XML.loadFile("./partner.wsdl")
		val schema = new Schema(wsdl, types)
		for (op <- (wsdl \ "portType" \ "operation")) {
			val opName = (op \ "@name").text
			val inMsg  = schema.messages(stripPrefix((op \ "input" \ "@message").text))
			val outMsg = schema.messages(stripPrefix((op \ "output" \ "@message").text))
			val inElm  = stripPrefix((inMsg \ "part" \ "@element").text)
			val outElm = stripPrefix((outMsg \ "part" \ "@element").text)
			println(opName.padTo(40, ' ') + inElm.padTo(40, ' ' ) + outElm)
			schema.handleInputElement(inElm)
			schema.handleOutputElement(outElm)
		}
		
		schema.writeTypes()
  	}
}

WSDL2ZKSforce.main(args)
