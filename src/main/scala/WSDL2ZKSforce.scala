// Copyright (c) 2013-2021 Simon Fell
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
package com.superfell.wsdl

import scala.xml._
import java.io._

class Operation(
    val name: String,
    val description: String,
    val params: collection.Seq[ComplexTypeProperty],
    val returnType: TypeInfo,
    val inputHeaders: collection.Seq[ComplexTypeProperty]
) {

  def requiresPrePostCallHooks(): Boolean = {
    name == "describeGlobal" || name == "describeSObject"
  }

  def objcSignature(): String = {
    s"-(${returnType.fullTypeName})$name${paramList}"
  }

  def preCallSyncHook(w: SourceWriter) {
    if (requiresPrePostCallHooks()) {
      w.println(
        s"""|	${returnType.fullTypeName}shortcut = [self preHook_${name}${callSyncParamList}];
						  |	if (shortcut != nil) return shortcut;""".stripMargin('|')
      )
    }
  }

  def postCallSyncHook(w: SourceWriter) {
    if (requiresPrePostCallHooks()) {
      w.println(s"""	result = [self postHook_${name}:result];""")
    }
  }

  def writeMakeEnvMethodImpl(w: SourceWriter) {
    w.println(makeEnvMethodSignature() + " {")
    w.println(
      "	ZKEnvelope *env = [[ZKPartnerEnvelope alloc] initWithSessionHeader:self.authSource.sessionId];"
    )
    w.writeFieldSerializers(
      "env",
      "self",
      inputHeaders.filter(_.elementName != "SessionHeader")
    )
    w.println(s"""|	[env moveToBody];
				      |	[env startElement:@"${name}"];""".stripMargin('|'))
    w.writeFieldSerializers("env", "", params)
    w.println(s"""|	[env endElement:@"${name}"];
					  |	return env.end;
					  |}""".stripMargin('|'))
  }

  def writeMakeResultMethodImpl(w: SourceWriter) {
    w.println(makeResultMethodSignature() + " {")
    val retStmt = returnType.accessor("deser", "result")
    if (retStmt != "") {
      w.println(
        s"""|	ZKElement *body = [root childElement:@"Body" ns:NS_SOAP_ENV];
				|	ZKXmlDeserializer *deser = [[ZKXmlDeserializer alloc] initWithXmlElement:body.childElements[0]];
				|	return $retStmt;""".stripMargin('|')
      )
    } else if (returnType.objcName != "void") {
      w.println(
        s"""	NSAssert(NO, @"subclass is expected to override this method");"""
      )
      w.println("	return nil;")
    }
    w.println("}")
  }

  def types(): collection.Seq[TypeInfo] = {
    returnType +: params.map(_.propType)
  }

  def paramList(): String = {
    if (params.length == 0) return ""
    val fp = params(0)
    val first = s":(${fp.propType.fullTypeName})${fp.propertyName}"
    if (params.length == 1) return first
    first + " " + params.tail.map(_.parameterDecl).mkString(" ")
  }

  // what we'd need to call the sync version of this operation, e.g. :soql or :soql foo:bar
  def callSyncParamList(): String = {
    if (params.length == 0) return ""
    val fp = params(0)
    val first = s":${fp.propertyName}"
    if (params.length == 1) return first;
    first + params.tail
      .map(x => (" " + x.propertyName + ":" + x.propertyName))
      .mkString("");
  }

  def makeEnvMethodName(): String = {
    s"""make${name.capitalize}Env"""
  }

  def makeEnvMethodSignature(): String = {
    s"""-(NSString *)${makeEnvMethodName}${paramList}"""
  }

  def makeResultMethodName(): String = {
    s"""make${name.capitalize}Result"""
  }

  def makeResultMethodSignature(): String = {
    s"""-(${returnType.fullTypeName})${makeResultMethodName}:(ZKElement *)root"""
  }

  def fullBlockMethodSignature(): String = {
    val cp = name.length
    if (params.length == 0)
      s"""/** ${description}
				|    Callbacks with be executed on the supplied queue. */
				|-(void) ${name}WithQueue:(dispatch_queue_t)callbackQueue 
				|${" ".padTo(cp + 8, ' ')}failBlock:(ZKFailWithErrorBlock)failBlock
				|${" ".padTo(
        cp + 4,
        ' '
      )}completeBlock:(${returnType.blockTypeName})completeBlock"""
        .stripMargin('|')
    else
      s"""/** ${description}
				|    Callbacks with be executed on the supplied queue. */
				|-(void) ${name}${paramList}
				|${" ".padTo(cp + 8 - 5, ' ')}queue:(dispatch_queue_t)callbackQueue
				|${" ".padTo(cp + 8 - 9, ' ')}failBlock:(ZKFailWithErrorBlock)failBlock
				|${" ".padTo(
        cp + 8 - 13,
        ' '
      )}completeBlock:(${returnType.blockTypeName})completeBlock"""
        .stripMargin('|')
  }

  def blockMethodSignature(): String = {
    val cp = name.length
    if (params.length == 0)
      s"""/** ${description}
				|    Callbacks will be executed on the main queue. */
				|-(void) ${name}WithFailBlock:(ZKFailWithErrorBlock)failBlock
				|${" ".padTo(
        cp + 8,
        ' '
      )}completeBlock:(${returnType.blockTypeName})completeBlock"""
        .stripMargin('|')
    else
      s"""/** ${description}
				|    Callbacks will be executed on the main queue. */
				|-(void) ${name}${paramList}
				|${" ".padTo(cp + 8 - 9, ' ')}failBlock:(ZKFailWithErrorBlock)failBlock
				|${" ".padTo(
        cp + 8 - 13,
        ' '
      )}completeBlock:(${returnType.blockTypeName})completeBlock"""
        .stripMargin('|')
  }
}

class Schema(wsdl: Elem, typeMapping: Map[String, TypeInfo]) {
  private val complexTypeElems = createComplexTypesElems(wsdl)
  private val complexTypes = collection.mutable.Map[String, ComplexTypeInfo]()
  private val elements = createElements(wsdl)
  private val simpleTypes = createSimpleTypes(wsdl)
  private val bindingOperations = createBindingOperations(wsdl)
  private val operations = collection.mutable.ListBuffer[Operation]()
  private var allHeaders = collection.mutable.ListBuffer[ComplexTypeProperty]()
  private val VOID = new VoidTypeInfo()

  val messages = createMessages(wsdl)
  createComplexTypes()

  def createComplexTypes() = {
    for ((name, _) <- complexTypeElems) {
      if (name != "QueryResult") {
        complexType(name)
      }
    }
  }

  def addOperation(
      name: String,
      inputElemName: String,
      outputElemName: String,
      description: String
  ) {
    val input = handleElement(inputElemName)
    val output = handleElement(outputElemName)
    var opType = if (output.length > 0) output(0).propType else VOID
    // headers
    val bindingOp = bindingOperations(inputElemName)
    val headers = (bindingOp \ "input" \ "header").map(x =>
      (handleHeader(
        qname.stripPrefix((x \ "@message").text),
        (x \ "@part").text
      ))
    )

    // some of the manually written operations have different return types to what's in the WSDL
    // so we need to fix up our metadata for that
    if (name == "describeGlobal")
      opType = new ArrayTypeInfo(
        getType("DescribeGlobalSObjectResult")
      )
    if (name == "retrieve")
      opType = new TypeInfo("dict", "NSDictionary", "", true)

    operations += new Operation(name, description, input, opType, headers)
  }

  def handleHeader(message: String, partName: String): ComplexTypeProperty = {
    val msg = messages(message)
    for (part <- msg \ "part") {
      if ((part \ "@name").text == partName) {
        val elmName = qname.stripPrefix((part \ "@element").text)
        val propName = elmName(0).toLower + elmName.substring(1)
        val elm = elements(elmName)
        if (!complexTypes.contains(elmName)) {
          makeComplexType(
            elmName,
            (elm \ "complexType")(0)
          )
          allHeaders += new ComplexTypeProperty(
            elmName,
            propName,
            complexTypes(elmName),
            false,
            true
          );
        }
        return new ComplexTypeProperty(
          elmName,
          propName,
          complexTypes(elmName),
          false,
          true
        );
      }
    }
    return null
  }

  private def handleElement(
      elementName: String
  ): collection.Seq[ComplexTypeProperty] = {
    // walk through the element decl and build the related types.
    val element = elements(elementName)
    return (element \ "complexType" \ "sequence" \ "element")
      .map(generateField(_))
  }

  def organize() {
    // sort headers by elementName
    allHeaders = allHeaders.sortWith(_.elementName < _.elementName)
  }

  def complexType(xmlName: String): ComplexTypeInfo = {
    val t = complexTypes.getOrElse(xmlName, makeComplexType(xmlName))
    return t
  }

  def writeClientStub() {
    new ASyncStubWriter(operations.toList).writeClass()
    new BaseClientWriter(operations.toList, allHeaders).writeClass()
    new BlockTypeDefStubWriter(operations.toList).writeClass()
  }

  def writeTypes() {
    for ((_, ct) <- complexTypes) {
      ct.writeHeaderFile()
      ct.writeImplFile()
    }
    for (h <- allHeaders)
      println("Header " + h.elementName)
  }

  def writeZKSforceHeader() {
    val w = new SourceWriter(new File(new File("output"), "ZKSforce.h"))
    w.printLicenseComment()
    val fixedImports = List(
      "ZKSforceClient.h",
      "ZKSforceBaseClient+Operations.h",
      "ZKSObject.h",
      "ZKLimitInfoHeader.h",
      "ZKConstants.h"
    )
    for (i <- fixedImports) {
      w.printImport(i)
    }
    w.printImports(complexTypes.values)
    val extraImports = List.from(
      new File("../zkSforce/zkSforce/extras")
        .listFiles()
        .filter(_.getName().contains("+"))
        .filter(_.getName().endsWith(".h"))
    )
    for (
      f <- extraImports.sortWith(
        _.getName().toLowerCase() < _.getName().toLowerCase()
      )
    ) {
      w.printImport(f.getName())
    }
    w.close()
  }

  private def createBindingOperations(wsdl: Elem): Map[String, Node] = {
    (wsdl \ "binding" \ "operation").map(x => ((x \ "@name").text, x)).toMap
  }

  private def createSimpleTypes(wsdl: Elem): Map[String, Node] = {
    val schemas = (wsdl \ "types" \ "schema")
    for (schema <- schemas) {
      if ((schema \ "@targetNamespace").text == "urn:partner.soap.sforce.com") {
        return (schema \ "simpleType").map(x => ((x \ "@name").text, x)).toMap
      }
    }
    return collection.immutable.Map[String, Node]()
  }

  private def createElements(wsdl: Elem): Map[String, Node] = {
    val schemas = (wsdl \ "types" \ "schema")
    for (schema <- schemas) {
      if ((schema \ "@targetNamespace").text == "urn:partner.soap.sforce.com") {
        return (schema \ "element").map(x => ((x \ "@name").text, x)).toMap
      }
    }
    return collection.immutable.Map[String, Node]()
  }

  private def createMessages(wsdl: Elem): Map[String, Node] = {
    (wsdl \ "message").map(x => ((x \ "@name").text, x)).toMap
  }

  private def createComplexTypesElems(wsdl: Elem): Map[String, Node] = {
    val schemas = (wsdl \ "types" \ "schema")
    for (schema <- schemas) {
      if ((schema \ "@targetNamespace").text == "urn:partner.soap.sforce.com") {
        return (schema \ "complexType").map(x => ((x \ "@name").text, x)).toMap
      }
    }
    return collection.immutable.Map[String, Node]()
  }

  private def makeObjcName(xmlName: String): String = {
    // There are some generated types that for legacy reasons we want to have a different name to the default name mapped from the wsdl
    val newNames = Map(
      // default Name		  			-> name to use instead
      "GetUserInfoResult" -> "ZKUserInfo",
      "Field" -> "ZKDescribeField",
      "DescribeGlobalSObjectResult" -> "ZKDescribeGlobalSObject",
      "DescribeSObjectResult" -> "ZKDescribeSObject"
    )
    return newNames.getOrElse(xmlName, "ZK" + xmlName.capitalize)
  }

  private def defaultComplexType(
      xmlName: String,
      objcName: String,
      ct: Node,
      fields: collection.Seq[ComplexTypeProperty],
      baseType: TypeInfo
  ): ComplexTypeInfo = {
    if (objcName == "ZKDescribeField")
      new ZKDescribeField(xmlName, objcName, ct, fields)
    else if (objcName == "ZKDescribeSObject") {
      val dg = complexType("DescribeGlobalSObjectResult");
      val childFields = fields.filter(!dg.fields.contains(_))
      new ZKDescribeSObject(xmlName, objcName, ct, childFields, dg)
    } else {
      new ComplexTypeInfo(xmlName, objcName, ct, fields, baseType)
    }
  }

  private def makeComplexType(
      xmlName: String
  ): ComplexTypeInfo = {
    val ct = complexTypeElems(xmlName)
    makeComplexType(xmlName, ct)
  }

  private def makeComplexType(
      xmlName: String,
      complexTypeNode: Node
  ): ComplexTypeInfo = {
    val objcName = makeObjcName(xmlName)
    // We insert a temporary version of the complexType to handle recursive definitions like DataCategory.
    // however this can leave fields with a propertyType that points to this temporary CTI, they aren't
    // fixed up later. This doesn't seem to be an issue currently, but may be in the future.
    complexTypes(xmlName) =
      new ComplexTypeInfo(xmlName, objcName, complexTypeNode, List(), null)
    println(xmlName)
    val base: String = qname.stripPrefix(
      (complexTypeNode \ "complexContent" \ "extension" \ "@base").text
    )
    val baseType: TypeInfo =
      if (base.length > 0) complexType(base) else null
    val sequence =
      if (base.length > 0)
        (complexTypeNode \ "complexContent" \ "extension" \ "sequence")
      else (complexTypeNode \ "sequence")
    val fields = (sequence \ "element").map(x => generateField(x))
    val i = defaultComplexType(
      xmlName,
      objcName,
      complexTypeNode,
      fields,
      baseType
    )
    complexTypes(xmlName) = i
    return i
  }

  private def generateField(
      field: Node
  ): ComplexTypeProperty = {
    val max = (field \ "@maxOccurs").text
    val array = (max != "" && max != "1")
    val xmlt = elementType(field)
    val name = (field \ "@name").text
    val singleType = getType(xmlt)
    val t = if (array) new ArrayTypeInfo(singleType) else singleType
    val optional = (field \ "@minOccurs").text == "0"
    val nillable = (field \ "@nillable").text == "true"
    new ComplexTypeProperty(name, name, t, nillable, optional)
  }

  private def getType(xmlName: String): TypeInfo = {
    if (typeMapping contains xmlName) return typeMapping(xmlName)
    if (complexTypes contains xmlName) return complexType(xmlName)
    if (simpleTypes contains xmlName)
      return typeMapping(
        "string"
      ) // are all simple types string extensions/restrictions ?
    makeComplexType(
      xmlName
    ) // no where else, assume its a complexType we haven't processed yet
  }

  private def elementType(e: Node): String = {
    qname.stripPrefix((e \ "@type").text)
  }
}

object qname {
  def stripPrefix(v: String): String = {
    val c = v.indexOf(':')
    if (c == -1) v else v.substring(c + 1)
  }
}

object WSDL2ZKSforce {
  def main(args: Array[String]) {
    // class TypeInfo(val xmlName: String, val objcName: String, accessor: String, val isPointer: Boolean)
    // accessor is the accessor method on the ZKXmlDeserializer class
    val types = Map(
      "string" -> new TypeInfo("string", "NSString", "string", true),
      "int" -> new TypeInfo("int", "NSInteger", "integer", false),
      "long" -> new TypeInfo("long", "int64_t", "int64", false),
      "double" -> new TypeInfo("double", "double", "double", false),
      "boolean" -> new TypeInfo("boolean", "BOOL", "boolean", false),
      "ID" -> new TypeInfo("ID", "NSString", "string", true),
      "sObject" -> new TypeInfo("sObject", "ZKSObject", "sObject", true),
      "QueryResult" -> new TypeInfo(
        "QueryResult",
        "ZKQueryResult",
        "queryResult",
        true
      ),
      "dateTime" -> new TypeInfo("dateTime", "NSDate", "dateTime", true),
      "date" -> new TypeInfo("date", "NSDate", "date", true),
      "time" -> new TypeInfo("time", "NSDate", "time", true),
      "base64Binary" -> new TypeInfo("base64Binary", "NSData", "blob", true),
      "anyType" -> new TypeInfo("anyType", "ZKXsdAnyType", "anyType", true)
    )

    val wsdl = XML.loadFile("./partner.wsdl")
    val schema = new Schema(wsdl, types)

    for (op <- (wsdl \ "portType" \ "operation")) {
      val opName = (op \ "@name").text
      val inMsg =
        schema.messages(qname.stripPrefix((op \ "input" \ "@message").text))
      val outMsg =
        schema.messages(qname.stripPrefix((op \ "output" \ "@message").text))
      val inElm = qname.stripPrefix((inMsg \ "part" \ "@element").text)
      val outElm = qname.stripPrefix((outMsg \ "part" \ "@element").text)
      val desc = (op \ "documentation").text
      println(opName.padTo(40, ' ') + inElm.padTo(40, ' ') + outElm)
      schema.addOperation(opName, inElm, outElm, desc)
    }

    schema.organize()
    schema.writeClientStub()
    schema.writeTypes()
    schema.writeZKSforceHeader()
  }
}
