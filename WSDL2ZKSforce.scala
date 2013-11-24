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

class SourceWriter(val file: File) {
	val w = new PrintWriter(file)

	def close() {
		w.close()
	}
	
	def println() {
		w.println()
	}
	
	def println(s: String) {
		w.println(s)
	}
	
	// for the collection of types, adds any import statements that would be needed.
	def printImports(types: Iterable[TypeInfo]) {
		val imports = types.map({
			_ match {
				case a:ArrayTypeInfo => if (a.componentType.isGeneratedType) a.componentType.objcName + ".h" else ""
				case t:TypeInfo      => if (t.isGeneratedType) t.objcName + ".h" else ""
			}
		})
		for (t <- imports.toSet.filter(_.length > 0))
			printImport(t)
	}
	
	def printImport(f: String) {
		w.println(s"""#import "$f"""")
	}
	
	def printClassForwardDecl(c: String) {
		w.println(s"@class $c;")
	}
	
	// reads the first line of the copyright from the current matching source file in the zkSforce tree, if it exists.
	private def getOriginalCopyright(file: File): String = {
		val src = new File(file.getParentFile(), "../../zkSforce/zkSforce/" + file.getName())
		if (!src.exists()) return null
		val line = scala.io.Source.fromFile(src.getAbsolutePath()).getLines().next
		if (line contains "Copyright") line else null
	}
	
	def printLicenseComment() {
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
}

class TypeInfo(val xmlName: String, val objcName: String, accessor: String, val isPointer: Boolean) {
	
	def propertyDeclComment(): String =  { "" }
	
	def propertyFlags(): String = { if (isPointer) "retain" else "assign" }
	
	def fullTypeName(): String = { if (isPointer) objcName + " *" else objcName }
	
	def isGeneratedType(): Boolean = { objcName.startsWith("ZK") }	// TODO
	
	def accessor(instanceName: String, elemName: String): String = {
		s"""[$instanceName $accessor:@"$elemName"]"""
	}
}

class ArrayTypeInfo(val componentType: TypeInfo) extends TypeInfo(componentType.xmlName, "NSArray", "", true) {

	override def propertyDeclComment(): String =  { " // of " + componentType.objcName }
	
	override def accessor(instanceName:String, elemName: String): String = {
		if (componentType.objcName == "NSString")
			s"""[$instanceName strings:@"$elemName"]"""
		else
			s"""[$instanceName complexTypeArrayFromElements:@"$elemName" cls:[${componentType.objcName} class]]"""
	}
}

class ComplexTypeProperty(val name: String, val propType: TypeInfo) {
	
	def readImplBody(): String =  {
		s"""-(${propType.fullTypeName})$name {
			|    return ${propType.accessor("self", name)};
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

class ComplexTypeInfo(xmlName: String, objcName: String, xmlNode: Node, fields: Seq[ComplexTypeProperty]) extends TypeInfo(xmlName, objcName, "", true) {
	
	val direction =  Direction.ValueSet.newBuilder

	private def validate() {
		val dir = direction.result()
		if (dir.contains(Direction.Deserialize) && dir.contains(Direction.Serialize))
			throw new RuntimeException("complexType with both serialization & deserialization not supported")
	}
	
	override def accessor(instanceName:String, elemName: String): String = {
		s"""[[$instanceName complexTypeArrayFromElements:@"$elemName" cls:[${objcName} class]] lastObject]"""
	}
	
	protected def headerImportFile(): String = { "" }
	protected def baseClass(): String = { "" }
	protected def includeIVarDecl(): Boolean = { false }
	protected def fieldsAreReadOnly(): Boolean = { true }
	protected def implementNSCopying(): Boolean = { false }
	
	def writeHeaderFile() {
		validate()

		val hfile = new File(new File("output"), objcName + ".h")
		hfile.getParentFile().mkdirs()
		val h = new SourceWriter(hfile)
		h.printLicenseComment()
		h.printImport(headerImportFile)
		h.println()
		writeForwardDecls(h)
		h.println("/*")
		val pp = new PrettyPrinter(809, 2)
		h.println(pp.format(xmlNode))
		h.println("*/")
		val nscopying = if (implementNSCopying()) "<NSCopying> " else ""
		h.println(s"@interface $objcName : ${baseClass} $nscopying{");
		writeHeaderIVars(h)
		h.println("}")
		writeHeaderProperties(h)
		h.println("@end")
		h.close()
	}

	protected def writeForwardDecls(w: SourceWriter) {
		for (f <- fields.filter(_.propType.isGeneratedType))
			w.printClassForwardDecl(f.propType.objcName)
	}
	
	private def padMembersTo(): Int = {
 		if (fields.length == 0) 0 else fields.map(_.propType.fullTypeName.length).max + 1
	}
	
	protected def writeHeaderIVars(w: SourceWriter) {
		if (includeIVarDecl)
			for (f <- fields)
				w.println(f.ivarDecl(padMembersTo))
	}

	protected def writeHeaderProperties(w: SourceWriter) {
		val padTo = padMembersTo
		for (f <- fields)
			w.println(f.propertyDecl(padTo, fieldsAreReadOnly))
	}
	
	protected def writeImplFileBody(w: SourceWriter) {} 
	
	protected def writeImplImports(w: SourceWriter) {}
		
	def writeImplFile() {
		val ifile = new File(new File("output"), objcName + ".m")
		val w = new SourceWriter(ifile)
		w.printLicenseComment()
		w.printImport(objcName + ".h")
		writeImplImports(w)
		w.println()
		w.println(s"@implementation $objcName")
		w.println()
		writeImplFileBody(w)
		w.println("@end")
		w.close()
	}
}

// A ComplexType from a message input, i.e. something we'll need to be able to serialize
class InputComplexTypeInfo(xmlName: String, objcName: String, xmlNode: Node, fields: Seq[ComplexTypeProperty]) extends ComplexTypeInfo(xmlName, objcName, xmlNode, fields) {

	override def headerImportFile(): String = { "ZKXMLSerializable.h" }
	override def baseClass(): String = { "NSObject<ZKXMLSerializable>" }
	override def includeIVarDecl(): Boolean = { true }
	override def fieldsAreReadOnly(): Boolean = { false }
	
	private def addLength(f: ComplexTypeProperty): Int = {
		f.name.length + f.serializeMethodName.length
	}
	
	override protected def writeImplImports(w: SourceWriter) {
		w.printImport("zkEnvelope.h")
	}

	override protected def writeImplFileBody(w: SourceWriter) {
		w.println("@synthesize " + fields.map(_.name).mkString(", ") + ";")
		w.println()
		w.println("-(void)dealloc {")
		for (f <- fields.filter(_.propType.isPointer))
			w.println(s"\t[${f.name} release];")
		w.println("\t[super dealloc];")
		w.println("}")
		w.println("-(void)serializeToEnvelope:(ZKEnvelope *)env elemName:(NSString *)elemName {")
		w.println("\t[env startElement:elemName];")
		val padTo = if (fields.length > 0) fields.map(addLength(_)).max else 0
		for (f <- fields) {
			val addMethod = f.serializeMethodName
			val pad = padTo - addMethod.length + 1
			val name = (f.name + "\"").padTo(pad, ' ')
			w.println(s"""\t[env $addMethod:@"$name elemValue:self.${f.name}];""")
		}
		w.println("\t[env endElement:elemName];")
		w.println("}")
	} 
}

// A ComplexType from a message output, i.e. something we'll need to be able to deserialize
class OutputComplexTypeInfo(xmlName: String, objcName: String, xmlNode: Node, fields: Seq[ComplexTypeProperty]) extends ComplexTypeInfo(xmlName, objcName, xmlNode, fields) {
	
	override def headerImportFile(): String = { "zkXmlDeserializer.h" }
	override def baseClass(): String = { "ZKXmlDeserializer" }
	
	override protected def writeImplImports(w: SourceWriter) {
		w.printImports(fields.map(_.propType))
	}
	
	protected def additionalNSCopyImpl(): String = { "" }
	 
	override protected def writeImplFileBody(w: SourceWriter) {
		if (implementNSCopying) {
			w.println(s"""-(id)copyWithZone:(NSZone *)zone {
				|    zkElement *e = [[node copyWithZone:zone] autorelease];
				|    $objcName *c = [[$objcName alloc] initWithXmlElement:e];
				|    ${additionalNSCopyImpl}
				|    return c;
				|}
				|
				|-(zkElement *)node {
				|	return node;
				|}
				|
				|-(BOOL)isEqual:(id)anObject {
				|	if (![anObject isKindOfClass:[$objcName class]]) return NO;
				|	return [node isEqual:[anObject node]];
				|}
				|
				|-(NSUInteger)hash {
				|	return [node hash];
				|}
				|""".stripMargin('|'))
		}
		for (f <- fields)
			w.println(f.readImplBody)
	}
}

class ZKDescribeField(xmlName:String, objcName:String, xmlNode:Node, fields:Seq[ComplexTypeProperty]) extends OutputComplexTypeInfo(xmlName, objcName, xmlNode, fields) {
	
	override protected def implementNSCopying(): Boolean = { true }
	
	override protected def writeForwardDecls(w: SourceWriter) {
		w.printClassForwardDecl("ZKDescribeSObject")
		super.writeForwardDecls(w)
	}
	
	override protected def writeHeaderIVars(w: SourceWriter) {
		w.println("\tZKDescribeSObject *sobject;")
	}
	
	override protected def writeHeaderProperties(w: SourceWriter) {
		w.println("@property (assign) ZKDescribeSObject *sobject; // assign to stop a ref counting loop")
		w.println()
		super.writeHeaderProperties(w)
	}
	
	override protected def writeImplImports(w: SourceWriter) {
		w.printImport("ZKDescribeSObject.h")
		super.writeImplImports(w)
	}
	
	override protected def additionalNSCopyImpl(): String = { "c.sobject = self.sobject;" }
	
	override protected def writeImplFileBody(w: SourceWriter) {
		w.println("@synthesize sobject;")
		w.println()
		super.writeImplFileBody(w)
	}
}

class VoidTypeInfo() extends TypeInfo("void", "void", "", false) {
	override def accessor(instanceName:String, elemName:String) : String = { "" }
}

class OperationParameter(val name: String, val paramType: TypeInfo) {
	def decl(): String = {
		s"$name:(${paramType.fullTypeName})$name"
	}
	
	def printAddElement(w: SourceWriter) {
		w.println(s"""	[env addElement:@"${name}" elemValue:${name}];""")
	}
}

class Operation(val name: String, val description: String, val params: Seq[OperationParameter], val returnType:TypeInfo) {
	
	def objcSignature(): String = {
		s"-(${returnType.fullTypeName})$name${paramList}"
	}
	
	def writeMethodDecl(w: SourceWriter) {
		w.println(s"""// $description
					|$objcSignature;
					|""".stripMargin('|'))
	}
	
	def writeMethodImpl(w: SourceWriter) {
		val nullValue = if (returnType.objcName == "void") "" else "nil"
		w.println(s"""// $description
					|$objcSignature {
					|	if (!authSource) return $nullValue;
					|	[self checkSession];
					|	ZKEnvelope *env = [[[ZKPartnerEnvelope alloc] initWithSessionHeader:[authSource sessionId] clientId:clientId] autorelease];
				    |	[env startElement:@"${name}"];""".stripMargin('|'));
		for (p <- params)
			p.printAddElement(w);
		val retStmt = returnType.accessor("deser", "result")
		w.println(s"""|	[env endElement:@"${name}"];
					 |	[env endElement:@"s:Body"];
					 |	zkElement *rn = [self sendRequest:[env end]];
					 |	ZKXmlDeserializer *deser = [[[ZKXmlDeserializer alloc] initWithXmlElement:rn] autorelease];
					 |	return $retStmt;
					 |}
					 |""".stripMargin('|'))
	}
	
	def types():Seq[TypeInfo] = {
		returnType +: params.map(_.paramType) 
	}
	
	def paramList():String = {
		if (params.length == 0) return ""
		val fp = params(0)
		val first = s":(${fp.paramType.fullTypeName})${fp.name}"
		if (params.length == 1) return first
		first + " " + params.tail.map(_.decl).mkString(" ")
	}
}

class StubWriter(allOperations: Seq[Operation]) {

	val operations = allOperations.filter(skipOperation(_))

	def writeStubClass() {
		writeStubHeader()
		writeStubImpl()
	}
	
	def skipOperation(op: Operation): Boolean = {
		return op.name != "login"
	}
	
	def referencedTypes(): Set[TypeInfo] = {
		Set(operations.map(_.types).flatten : _*)
	}
	
	def writeStubHeader() {
		val w= new SourceWriter(new File(new File("output"), "zkSforceClient+Operations.h"))
		w.printLicenseComment()
		w.printImport("zkSforce.h")
		w.println()
		for (t <- referencedTypes.filter(_.isGeneratedType))
			w.printClassForwardDecl(t.objcName)
		w.println()
		w.println("@interface ZKSforceClient (Operations)")
		for (op <- operations)
			op.writeMethodDecl(w)
		w.println("@end")
		w.close()
	}
	
	def writeStubImpl() {
		val w = new SourceWriter(new File(new File("output"), "zkSforceClient+Operations.m"))
		w.printLicenseComment();
		w.printImport("zkSforceClient+Operations.h")
		w.printImports(referencedTypes)
		w.println()
		w.println("@implementation ZKSforceClient (Operations)")
		for (op <- operations)
			op.writeMethodImpl(w)
		w.println("@end")
		w.close()
	}
}

class Schema(wsdl: Elem, typeMapping: Map[String, TypeInfo]) {
	private val complexTypeElems = createComplexTypesElems(wsdl)
	private val complexTypes = collection.mutable.Map[String, ComplexTypeInfo]()
	private val elements = createElements(wsdl)
	private val simpleTypes = createSimpleTypes(wsdl)
	private val operations = collection.mutable.MutableList[Operation]()
	private val VOID = new VoidTypeInfo()
	
	val messages = createMessages(wsdl)

	def addOperation(name: String, inputElemName: String, outputElemName: String, description: String) {
		val input = handleInputElement(inputElemName)
		val output = handleOutputElement(outputElemName)
		val opType = if (output.length > 0) output(0).propType else VOID
		operations += new Operation(name, description, input.map(convertToParam(_)), opType)
	}
	
	def handleInputElement(elementName: String): Seq[ComplexTypeProperty] = {
		return handleElement(elementName, Direction.Serialize)
	}
	
	def handleOutputElement(elementName: String): Seq[ComplexTypeProperty] = {
		return handleElement(elementName, Direction.Deserialize)
	}
	
	private def convertToParam(prop: ComplexTypeProperty): OperationParameter = {
		return new OperationParameter(prop.name, prop.propType)
	}
	
	private def handleElement(elementName: String, dir: Direction.Value): Seq[ComplexTypeProperty] = {
		// walk through the element decl and build the related types.
		val element = elements(elementName)
		return (element \ "complexType" \ "sequence" \ "element").map(generateField(_, dir))
	}
	
	def complexType(xmlName: String, dir: Direction.Value) : ComplexTypeInfo = {
		val t = complexTypes.getOrElse(xmlName, makeComplexType(xmlName, dir))
		t.direction += dir
		return t
	}
	
	def writeClientStub() {
		new StubWriter(operations).writeStubClass()
	}
	
	def writeTypes() {
		for ((_, ct) <- complexTypes) {
			ct.writeHeaderFile()
			ct.writeImplFile()
		}
	}
	
    def writeZKSforceh() {
		val w = new SourceWriter(new File(new File("output"), "zkSforce.h"))
		w.printLicenseComment()
		val fixedImports = List("zkSforceClient.h", "zkSObject.h", "zkSoapException.h")
		for (i <- fixedImports)
			w.printImport(i)
		for ((_, ct) <- complexTypes)
			if (ct.isGeneratedType)
				w.printImport(ct.objcName + ".h")
		for(f <- new File("../zkSforce/zkSforce").listFiles().filter(_.getName().contains("+")).filter(_.getName().endsWith(".h")))
			w.printImport(f.getName())
		w.close()
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
	
	
	private def makeObjcName(xmlName: String): String = {
		// There are some generated types that for legacy reasons we want to have a different name to the default name mapped from the wsdl
		val newNames = Map(
						// default Name		  -> name to use instead
						"GetUserInfoResult" -> "ZKUserInfo",
						"Field"	  		    -> "ZKDescribeField"
						)
		return newNames.getOrElse(xmlName, "ZK" + xmlName.capitalize)
	}

	private def defaultComplexType(dir: Direction.Value, xmlName: String, objcName: String, ct: Node, fields: Seq[ComplexTypeProperty]): ComplexTypeInfo = {
		if (objcName == "ZKDescribeField")
			new ZKDescribeField(xmlName, objcName, ct, fields);
		else if (dir == Direction.Serialize)
			new InputComplexTypeInfo(xmlName, objcName, ct, fields) 
		else
			new OutputComplexTypeInfo(xmlName, objcName, ct, fields)
	}
	
	private def makeComplexType(xmlName: String, dir: Direction.Value): ComplexTypeInfo = {
		val ct = complexTypeElems(xmlName)
		val objcName = makeObjcName(xmlName)
		// we insert a temporary version of the complexType to handle recursive definitions
		complexTypes(xmlName) = new ComplexTypeInfo(xmlName, objcName, ct, List())
		val fields = (ct \ "sequence" \ "element").map( x => generateField(x, dir) )
		val i = defaultComplexType(dir, xmlName, objcName, ct, fields)
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
					"string" 		-> new TypeInfo("string", 		"NSString",  	"string",  		true),
					"int" 	 		-> new TypeInfo("int",    		"NSInteger", 	"integer", 		false),
					"boolean"		-> new TypeInfo("boolean", 		"BOOL", 	 	"boolean", 		false),
					"ID"	 		-> new TypeInfo("ID",			"NSString",  	"string",  		true),
					"sObject"		-> new TypeInfo("sObject", 		"ZKSObject", 	"sObject",  	true),
					"QueryResult"   -> new TypeInfo("QueryResult",  "ZKQueryResult","queryResult",	true),
					"dateTime"		-> new TypeInfo("dateTime",		"NSDate",  	 	"dateTime", 	true),
					"date"   		-> new TypeInfo("date",    		"NSDate",    	"date",     	true),
					"base64Binary" 	-> new TypeInfo("base64Binary", "NSData", 	 	"blob",     	true)
					)
					
		val wsdl = XML.loadFile("./partner.wsdl")
		val schema = new Schema(wsdl, types)
		for (op <- (wsdl \ "portType" \ "operation")) {
			val opName = (op \ "@name").text
			val inMsg  = schema.messages(stripPrefix((op \ "input" \ "@message").text))
			val outMsg = schema.messages(stripPrefix((op \ "output" \ "@message").text))
			val inElm  = stripPrefix((inMsg \ "part" \ "@element").text)
			val outElm = stripPrefix((outMsg \ "part" \ "@element").text)
			val desc   = (op \ "documentation").text
			println(opName.padTo(40, ' ') + inElm.padTo(40, ' ' ) + outElm)
			schema.addOperation(opName, inElm, outElm, desc)
		}

		schema.writeClientStub()
		schema.writeTypes()
		schema.writeZKSforceh()
  	}
}

WSDL2ZKSforce.main(args)
