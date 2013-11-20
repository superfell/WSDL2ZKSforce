// Copyright (c) 2013 Simon Fell
//
// Permission is hereby granted, free of charge, to any person obtaining a 
// copy of this software and associated documentation files (the "Software"), 
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense, 
// and/or sell copies of the Software, and to permit persons to whom the 
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included 
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN 
// THE SOFTWARE.
//

// This is not a general purpose WSDL2Objc generator, its for generating
// classes for inclusion with ZKSforce when a new WSDL version comes out
// If you want to use ZKSforce you DO NOT HAVE TO RUN THIS, the relevant
// classes are already in ZKSforce.

import scala.xml._
import java.io._

class TypeInfo(val xmlName: String, val objcName: String, val propertyFlags: String) {
	
	def propertyDeclComment(): String =  { "" }
}

class ArrayTypeInfo(val componentType: TypeInfo) extends TypeInfo(componentType.xmlName, "NSArray *", "retain") {

	override def propertyDeclComment(): String =  { " // of " + componentType.objcName }
}

class ComplexTypeProperty(val name: String, val propType: TypeInfo) {
	
	def propertyDecl(readOnly: Boolean): String = {
		val f = if (readOnly) "readonly" else propType.propertyFlags
		val t = propType.objcName
		val comment = propType.propertyDeclComment
		s"@property ($f) $t $name; $comment"
	}
}

object Direction extends Enumeration {
	val Serialize, Deserialize = Value
}

class ComplexTypeInfo(xmlName: String, fields: Seq[ComplexTypeProperty]) extends TypeInfo(xmlName, "ZK" + xmlName.capitalize, "retain") {
	
	val direction =  Direction.ValueSet.newBuilder
		
	def writeHeaderFile() {
		val dir = direction.result()
		val deserializer = dir.contains (Direction.Deserialize)
		if (dir.contains(Direction.Deserialize) && dir.contains(Direction.Serialize))
			throw new RuntimeException("complexType with both serialization & deserialization not supported")

		val hfile = new File(new File("output"), objcName + ".h")
		hfile.getParentFile().mkdirs()
		val h = new PrintWriter(hfile)
		writeComment(h)
		val importFile = if (deserializer) "zkDeserializer.h" else "ZKXMLSerializable.h"
		val baseClass  = if (deserializer) "ZKXMLDeserializer" else "NSObject<XMLSerializable>"
		h.println((s"""#import $importFile
					|
					|@interface $objcName : $baseClass {
					|}
					""").stripMargin('|'))
		for (f <- fields)
			h.println(f.propertyDecl(deserializer))
		h.println("@end")
		h.close()
	}
	
	private def writeComment(w: PrintWriter) {
		w.println("""/// Copyright (c) 2006-2013 Simon Fell
		///
		/// Permission is hereby granted, free of charge, to any person obtaining a 
		/// copy of this software and associated documentation files (the "Software"), 
		/// to deal in the Software without restriction, including without limitation
		/// the rights to use, copy, modify, merge, publish, distribute, sublicense, 
		/// and/or sell copies of the Software, and to permit persons to whom the 
		/// Software is furnished to do so, subject to the following conditions:
		///
		/// The above copyright notice and this permission notice shall be included 
		/// in all copies or substantial portions of the Software.
		///
		/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
		/// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
		/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
		/// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
		/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
		/// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN 
		/// THE SOFTWARE.
		///
		/""".stripMargin('/'));
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
