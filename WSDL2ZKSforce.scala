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

class TypeInfo(val xmlName: String, val objcName: String, accessor: String, val isPointer: Boolean) {
	
	def propertyDeclComment(): String =  { "" }
	
	def propertyFlags(): String = { if (isPointer) "retain" else "assign" }
	
	def fullTypeName(): String = { if (isPointer) objcName + " *" else objcName }
	
	def isGeneratedType(): Boolean = { objcName.startsWith("ZK") }	// TODO
	
	def accessor(elemName: String): String = {
		s"""[self $accessor:@"$elemName"]"""
	}
}

class ArrayTypeInfo(val componentType: TypeInfo) extends TypeInfo(componentType.xmlName, "NSArray", "", true) {

	override def propertyDeclComment(): String =  { " // of " + componentType.objcName }
	
	override def accessor(elemName: String): String = {
		if (componentType.objcName == "NSString")
			s"""[self strings:@"$elemName"]"""
		else
			s"""[self complexTypeArrayFromElements:@"$elemName" cls:[${componentType.objcName} class]]"""
	}
}

class ComplexTypeProperty(val name: String, val propType: TypeInfo) {
	
	def readImplBody(): String =  {
		s"""-(${propType.fullTypeName})$name {
			|    return ${propType.accessor(name)};
			|}
			""".stripMargin('|')
	}
	
	def propertyDecl(padTypeTo: Int, readOnly: Boolean): String = {
		val f = if (readOnly) "readonly" else propType.propertyFlags
		val comment = propType.propertyDeclComment
		val td = typeDef(padTypeTo)
		s"@property ($f) $td; $comment"
	}
	
	def ivarDecl(padTypeTo: Int): String = {
		val td = typeDef(padTypeTo)
		s"\t$td;"
	}
	
	def serializeMethodName(): String = {
		if (propType.objcName == "BOOL") "addBoolElement" else "addElement"
	}
	
	private def typeDef(padTypeTo: Int): String = {
		val t = propType.objcName.padTo(padTypeTo - (if (propType.isPointer) 1 else 0), ' ')
		val p = if (propType.isPointer) "*" else ""
		s"$t$p$name"
	}
}

object Direction extends Enumeration {
	val Serialize, Deserialize = Value
}

class ComplexTypeInfo(xmlName: String, xmlNode: Node, fields: Seq[ComplexTypeProperty]) extends TypeInfo(xmlName, "ZK" + xmlName.capitalize, "", true) {
	
	val direction =  Direction.ValueSet.newBuilder

	private def validate() {
		val dir = direction.result()
		if (dir.contains(Direction.Deserialize) && dir.contains(Direction.Serialize))
			throw new RuntimeException("complexType with both serialization & deserialization not supported")
	}
	
	override def accessor(elemName: String): String = {
		s"""[[self complexTypeArrayFromElements:@"$elemName" cls:[${objcName} class]] lastObject]"""
	}
	
	def headerImportFile(): String = { "" }
	def baseClass(): String = { "" }
	def includeIVarDecl(): Boolean = { false }
	def fieldsAreReadOnly(): Boolean = { true }
	
	def writeHeaderFile() {
		validate()

		val hfile = new File(new File("output"), objcName + ".h")
		hfile.getParentFile().mkdirs()
		val h = new PrintWriter(hfile)
		writeComment(h, hfile)
		h.println(s"""#import "$headerImportFile"""")
		h.println()
		for (f <- fields.filter(_.propType.isGeneratedType))
			h.println(s"@class ${f.propType.objcName};")
		h.println("/*")
		val pp = new PrettyPrinter(809, 2)
		h.println(pp.format(xmlNode))
		h.println("*/")
		h.println(s"@interface $objcName : ${baseClass} {");
		val padTo = if (fields.length == 0) 0 else fields.map(_.propType.fullTypeName.length).max + 1
		if (includeIVarDecl)
			for (f <- fields)
				h.println(f.ivarDecl(padTo))
		h.println("}")
		for (f <- fields)
			h.println(f.propertyDecl(padTo, fieldsAreReadOnly))
		h.println("@end")
		h.close()
	}
	
	protected def writeImplFileBody(w: PrintWriter) {} 
	
	protected def writeImports(w: PrintWriter) {}
		
	def writeImplFile() {
		val ifile = new File(new File("output"), objcName + ".m")
		val w = new PrintWriter(ifile)
		writeComment(w, ifile)
		w.println(s"""#import "$objcName.h"""")
		writeImports(w)
		w.println()
		w.println(s"@implementation $objcName")
		w.println()

		writeImplFileBody(w)
			
		w.println("@end")
		w.close()
	}
	
	private def writeComment(w: PrintWriter, file: File) {
		val year = java.util.Calendar.getInstance().get(java.util.Calendar.YEAR)
		val defaultCopyright = s"// Copyright (c) $year Simon Fell"
		val originalCopyright = getOriginalCopyright(file)
		val copyRight = if (originalCopyright == null) defaultCopyright else originalCopyright
		w.println(copyRight)
		w.println("""///
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
	
	// reads the first line of the copyright from the current matching source file in the zkSforce tree, if it exists.
	private def getOriginalCopyright(file: File): String = {
		val src = new File(file.getParentFile(), "../../zkSforce/zkSforce/" + file.getName())
		if (!src.exists()) return null
		val line = scala.io.Source.fromFile(src.getAbsolutePath()).getLines().next
		if (line contains "Copyright") line else null
	}
}

// A ComplexType from a message input, i.e. something we'll need to be able to serialize
class InputComplexTypeInfo(xmlName: String, xmlNode: Node, fields: Seq[ComplexTypeProperty]) extends ComplexTypeInfo(xmlName, xmlNode, fields) {

	override def headerImportFile(): String = { "ZKXMLSerializable.h" }
	override def baseClass(): String = { "NSObject<XMLSerializable>" }
	override def includeIVarDecl(): Boolean = { true }
	override def fieldsAreReadOnly(): Boolean = { false }
	
	private def addLength(f: ComplexTypeProperty): Int = {
		f.name.length + f.serializeMethodName.length
	}
	
	override protected def writeImplFileBody(w: PrintWriter) {
		w.println("@synthesize " + fields.map(_.name).mkString(", ") + ";")
		w.println()
		w.println("-(void)dealloc {")
		for (f <- fields.filter(_.propType.isPointer))
			w.println(s"\t[${f.name} release];")
		w.println("\t[super dealloc]")
		w.println("}")
		w.println("-(void)serializeToEnvelope:(ZKEnvelope *)env elemName:(NSString *)elemName {")
		w.println("\t[env startElement:elemName]")
		val padTo = if (fields.length > 0) fields.map(addLength(_)).max else 0
		for (f <- fields) {
			val addMethod = f.serializeMethodName
			val pad = padTo - addMethod.length + 1
			val name = (f.name + "\"").padTo(pad, ' ')
			w.println(s"""\t[env $addMethod:@"$name elemValue:self.${f.name}];""")
		}
		w.println("\t[env endElement:elemName]")
		w.println("}")
	} 
}

// A ComplexType from a message output, i.e. something we'll need to be able to deserialize
class OutputComplexTypeInfo(xmlName: String, xmlNode: Node, fields: Seq[ComplexTypeProperty]) extends ComplexTypeInfo(xmlName, xmlNode, fields) {
	
	override def headerImportFile(): String = { "zkXmlDeserializer.h" }
	override def baseClass(): String = { "ZKXmlDeserializer" }
	
	override protected def writeImports(w: PrintWriter) {
		for (f <- fields) {
			val importStmt = f.propType match {
				case a:ArrayTypeInfo => if (a.componentType.isGeneratedType) s"""#import "${a.componentType.objcName}.h"""" else ""
				case t:TypeInfo      => if (t.isGeneratedType) s"""#import "${t.objcName}.h"""" else ""
			}
			if (importStmt.length > 0)
				w.println(importStmt)
		}
	}
	
	override protected def writeImplFileBody(w: PrintWriter) {
		for (f <- fields)
			w.println(f.readImplBody)
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
		for ((_, ct) <- complexTypes) {
			ct.writeHeaderFile()
			ct.writeImplFile()
		}
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
		complexTypes(xmlName) = new ComplexTypeInfo(xmlName, ct, List())
		val fields = (ct \ "sequence" \ "element").map( x => generateField(x, dir) )
		val i = if (dir == Direction.Serialize) new InputComplexTypeInfo(xmlName, ct, fields) else new OutputComplexTypeInfo(xmlName, ct, fields)
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
					"string" 		-> new TypeInfo("string", 		"NSString",  "string",  true),
					"int" 	 		-> new TypeInfo("int",    		"NSInteger", "integer", false),
					"boolean"		-> new TypeInfo("boolean", 		"BOOL", 	 "boolean", false),
					"ID"	 		-> new TypeInfo("ID",			"NSString",  "string",  true),
					"sObject"		-> new TypeInfo("sObject", 		"ZKSObject", "sObject",  true),
					"dateTime"		-> new TypeInfo("dateTime",		"NSDate",  	 "dateTime", true),
					"date"   		-> new TypeInfo("date",    		"NSDate",    "date",     true),
					"base64Binary" 	-> new TypeInfo("base64Binary", "NSData", 	 "blob",     true)
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
