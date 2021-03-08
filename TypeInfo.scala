package com.superfell.wsdl

import scala.xml._
import java.io._

// Each mapping from an XML based Type to an Objective-C type we need to generate is represented by a TypeInfo
// TypeInfo has a hierarchy for different types of types (e.g. arrays, classes, etc)
class TypeInfo(
    val xmlName: String,
    val objcName: String,
    accessor: String,
    val isPointer: Boolean
) {

  // additional comment that'll get added to a property declaration of this type.
  def propertyDeclComment(): String = { "" }

  // what property flags should be used for this type.
  def propertyFlags(): String = { if (isPointer) "strong" else "assign" }

  // what property flags should be used for a readonly property of this type
  def readonlyPropertyFlags(): String = {
    if (isPointer) "weak, readonly" else "readonly"
  }

  def propertyLengthForPaddingCalc(includeFlags: Boolean): Int = {
    var l = fullTypeName.length
    if (includeFlags) l += readonlyPropertyFlags.length
    return l
  }

  // full name of the objective-c type, includes * for pointer types.
  def fullTypeName(): String = { if (isPointer) objcName + " *" else objcName }

  // returns true if this type is one we generated rather than a system type
  def isGeneratedType(): Boolean = { objcName.startsWith("ZK") } // TODO

  // returns an accessor that returns an instance of this type (when deserializing xml)
  def accessor(instanceName: String, elemName: String): String = {
    if (accessor != "") {
      s"""[$instanceName $accessor:@"$elemName"]"""
    } else {
      s""
    }
  }

  // returns the name of the method that is used to serialize an instance of this type
  def serializerMethodName(): String = {
    if (objcName == "BOOL") "addBoolElement"
    else if (objcName == "NSInteger") "addIntElement"
    else if (objcName == "double") "addDoubleElement"
    else "addElement"
  }

  // type name for use with blocks
  def blockTypeName(): String = {
    return "ZKComplete" + objcName.substring(2) + "Block"
  }
}

// For types that are mapped to Arrays.
class ArrayTypeInfo(val componentType: TypeInfo)
    extends TypeInfo(componentType.xmlName, "NSArray", "", true) {

  override def propertyDeclComment(): String = {
    " // of " + componentType.objcName
  }

  override def accessor(instanceName: String, elemName: String): String = {
    if (componentType.objcName == "NSString")
      s"""[$instanceName strings:@"$elemName"]"""
    else
      s"""[$instanceName complexTypeArrayFromElements:@"$elemName" cls:[${componentType.objcName} class]]"""
  }

  override def serializerMethodName(): String = {
    "addElementArray"
  }
}

object ComplexTypeProperty {
  val reservedWords = Set("inline")
}

// A property of a complexType (aka Class)
class ComplexTypeProperty(
    elemName: String,
    propName: String,
    val propType: TypeInfo,
    val nillable: Boolean,
    val optional: Boolean
) {

  val elementName = elemName
  val propertyName = makePropertyName(elemName, propName)

  private def makePropertyName(elemName: String, propName: String): String = {
    val n = if (propName == "") elemName else propName
    if (ComplexTypeProperty.reservedWords.contains(n)) "_" + n else n
  }

  def initializer(nameOfZKElementInstance: String): String = {
    val zke = nameOfZKElementInstance
    s"\tself.$propertyName = ${propType.accessor(zke, elementName)};"
  }

  def readImplBody(): String = {
    s"""-(${propType.fullTypeName})$propertyName {
			|    return ${propType.accessor("self", elementName)};
			|}
			""".stripMargin('|')
  }

  def propertyDecl(padTypeTo: Int, readOnly: Boolean): String = {
    val f =
      if (readOnly) propType.readonlyPropertyFlags else propType.propertyFlags
    val comment = propType.propertyDeclComment
    val padTo = if (readOnly) padTypeTo - f.length else padTypeTo
    val td = typeDef(padTo)
    val nr =
      if (propertyName.startsWith("new")) " NS_RETURNS_NOT_RETAINED" else ""
    var nrc =
      if (nr.length > 0)
        "; returns an autoreleased object, doesn't follow cocoa rules for properties/method starting with 'new'"
      else ""
    s"@property ($f) $td$nr; $comment$nrc"
  }

  def ivarDecl(padTypeTo: Int): String = {
    val td = typeDef(padTypeTo)
    s"\t$td;"
  }

  def parameterDecl(): String = {
    s"$propertyName:(${propType.fullTypeName})$propertyName"
  }

  private def typeDef(padTypeTo: Int): String = {
    val t = propType.objcName.padTo(
      padTypeTo - (if (propType.isPointer) 1 else 0),
      ' '
    )
    val p = if (propType.isPointer) "*" else ""
    s"$t$p$propertyName"
  }

  override def equals(other: Any): Boolean = {
    if (!other.isInstanceOf[ComplexTypeProperty]) return false
    val r = other.asInstanceOf[ComplexTypeProperty]
    r.propertyName == propertyName && r.propType.objcName == propType.objcName
  }

  // return the length of the serializer method name + the length of the element name, this is used to calc the right padding for a set of properties
  def serializerLength(): Integer = {
    return propType.serializerMethodName.length + elementName.length + 1
  }

  private def objcBool(v: Boolean): String = {
    if (v) "YES" else "NO"
  }

  def serializerMethod(
      instName: String,
      padTo: Integer,
      padNameTo: Integer,
      valueScope: String
  ): String = {
    val addMethod = propType.serializerMethodName
    val pad = " ".padTo(padTo - addMethod.length - elementName.length, ' ')
    val scope = if (valueScope.length > 0) valueScope + "." else ""
    val namePad = "".padTo(padNameTo - elementName.length, ' ')
    val extra =
      if (propType.serializerMethodName == "addElement")
        s"${namePad} nillable:${objcBool(nillable).padTo(3, ' ')} optional:${objcBool(optional)}"
      else
        ""
    s"""\t[$instName ${propType.serializerMethodName}:@"$elementName"${pad}elemValue:$scope$propertyName$extra];"""
  }
}

object Direction extends Enumeration {
  val Serialize, Deserialize = Value
}

// For types that are mapped from ComplexTypes (aka Classes)
class ComplexTypeInfo(
    xmlName: String,
    objcName: String,
    xmlNode: Node,
    val fields: collection.Seq[ComplexTypeProperty],
    baseType: TypeInfo
) extends TypeInfo(xmlName, objcName, "", true) {

  val direction = Direction.ValueSet.newBuilder

  def prevalidate() {
    // if we're In & Out, we need our bass classes to also be in/out
    val dir = direction.result();
    if (
      dir.contains(Direction.Deserialize) && dir.contains(Direction.Serialize)
    ) {
      if (baseType != null) {
        baseType match {
          case c: ComplexTypeInfo => {
            c.direction += Direction.Serialize;
            c.direction += Direction.Deserialize; c.prevalidate();
          }
        }
      }
    }
  }

  def validate(): ComplexTypeInfo = {
    val dir = direction.result()
    if (
      dir.contains(Direction.Deserialize) && dir.contains(Direction.Serialize)
    ) {
      print(s"Promoting type $xmlName to an InputOutputComplexType\n")
      return new InputOutputComplexTypeInfo(
        xmlName,
        objcName,
        xmlNode,
        fields,
        baseType
      )
    }
    this
  }

  override def accessor(instanceName: String, elemName: String): String = {
    s"""[$instanceName complexTypeArrayFromElements:@"$elemName" cls:[${objcName} class]].lastObject"""
  }

  protected def headerImportFile(): String = {
    if (baseType == null) "" else baseType.objcName + ".h"
  }
  protected def baseClass(): String = {
    if (baseType == null) "" else baseType.objcName
  }
  protected def includeIVarDecl(): Boolean = { false }
  protected def fieldsAreReadOnly(): Boolean = { true }
  protected def implementNSCopying(): Boolean = { false }

  def writeHeaderFile() {
    val hfile = new File(new File("output/generated"), objcName + ".h")
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
    for (
      t <- fields
        .filter(_.propType.isGeneratedType)
        .map(f => f.propType)
        .distinct
    ) {
      w.printClassForwardDecl(t.objcName)
    }
  }

  private def padMembersTo(includeFlags: Boolean): Int = {
    if (fields.length == 0) 0
    else
      fields.map(_.propType.propertyLengthForPaddingCalc(includeFlags)).max + 1
  }

  protected def writeHeaderIVars(w: SourceWriter) {}

  protected def writeHeaderProperties(w: SourceWriter) {
    val padTo = padMembersTo(fieldsAreReadOnly)
    for (f <- fields)
      w.println(f.propertyDecl(padTo, fieldsAreReadOnly))
  }

  protected def writeImplFileBody(w: SourceWriter) {}

  protected def writeImplImports(w: SourceWriter) {}

  def writeImplFile() {
    val ifile = new File(new File("output/generated"), objcName + ".m")
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
class InputComplexTypeInfo(
    xmlName: String,
    objcName: String,
    xmlNode: Node,
    fields: collection.Seq[ComplexTypeProperty],
    baseType: TypeInfo
) extends ComplexTypeInfo(xmlName, objcName, xmlNode, fields, baseType) {

  override def headerImportFile(): String = {
    val s = super.headerImportFile
    if (s == "") "ZKXMLSerializable.h" else s
  }

  override def baseClass(): String = {
    val s = super.baseClass
    if (s == "") "NSObject<ZKXMLSerializable>" else s
  }

  override def includeIVarDecl(): Boolean = { true }
  override def fieldsAreReadOnly(): Boolean = { false }

  override protected def writeImplImports(w: SourceWriter) {
    w.printImport("ZKEnvelope.h")
  }

  protected def writeExtraImpl(w: SourceWriter) {}

  override protected def writeImplFileBody(w: SourceWriter) {
    w.println("@synthesize " + fields.map(_.propertyName).mkString(", ") + ";")
    writeExtraImpl(w)
    w.println()
    w.println(
      "-(void)serializeTo:(ZKXmlWriter *)env elemName:(NSString *)elemName {"
    )
    // if there's a baseType, then this is an extension type, and we need to serialize our type out as an xsi:type attribute
    if (baseType == null)
      w.println("\t[env startElement:elemName];")
    else
      w.println("\t[env startElement:elemName type:@\"" + xmlName + "\"];")
    val fieldsToSerialize =
      if (baseType == null) fields
      else (baseType.asInstanceOf[ComplexTypeInfo].fields) ++ fields
    w.writeFieldSerializers("env", "self", fieldsToSerialize)
    w.println("\t[env endElement:elemName];")
    w.println("}")
  }
}

// A ComplexType from a message output, i.e. something we'll need to be able to deserialize
class OutputComplexTypeInfo(
    xmlName: String,
    objcName: String,
    xmlNode: Node,
    fields: collection.Seq[ComplexTypeProperty],
    baseType: TypeInfo
) extends ComplexTypeInfo(xmlName, objcName, xmlNode, fields, baseType) {

  override def headerImportFile(): String = {
    val s = super.headerImportFile
    if (s == "") "ZKXmlDeserializer.h" else s
  }

  override def baseClass(): String = {
    val s = super.baseClass
    if (s == "") "ZKXmlDeserializer" else s
  }

  override protected def writeImplImports(w: SourceWriter) {
    w.printImports(fields.map(_.propType))
  }

  protected def additionalNSCopyImpl(): String = { "" }

  override protected def writeImplFileBody(w: SourceWriter) {
    w.println(s"""+(void)load {
                |   [self registerType:self xmlName:@"$xmlName"];
                |}
                |
      """.stripMargin('|'))
    if (implementNSCopying) {
      w.println(s"""-(id)copyWithZone:(NSZone *)zone {
				|    ZKElement *e = [node copyWithZone:zone];
				|    $objcName *c = [[$objcName alloc] initWithXmlElement:e];
				|    ${additionalNSCopyImpl}
				|    return c;
				|}
				|
				|-(ZKElement *)node {
				|	return node;
				|}
				|
				|-(BOOL)isEqual:(id)anObject {
				|	if (![anObject isKindOfClass:[$objcName class]]) return NO;
				|	return [node isEqual:[anObject node]];
				|}
				|
				|-(NSUInteger)hash {
				|	return node.hash;
				|}
				|""".stripMargin('|'))
    }
    for (f <- fields)
      w.println(f.readImplBody)
  }
}

class InputOutputComplexTypeInfo(
    xmlName: String,
    objcName: String,
    xmlNode: Node,
    fields: collection.Seq[ComplexTypeProperty],
    baseType: TypeInfo
) extends InputComplexTypeInfo(xmlName, objcName, xmlNode, fields, baseType) {

  override def baseClass(): String = {
    if (baseType == null) "ZKXmlDeserializer<ZKXMLSerializable>"
    else super.baseClass()
  }

  override protected def writeForwardDecls(w: SourceWriter) {
    w.printImport("ZKXmlDeserializer.h")
    w.printImport("ZKParser.h")
  }

  override protected def writeHeaderProperties(w: SourceWriter) {
    w.println("-(instancetype)init NS_DESIGNATED_INITIALIZER;")
    w.println(
      "-(instancetype)initWithZKXmlDeserializer:(ZKXmlDeserializer *)d NS_DESIGNATED_INITIALIZER;"
    );
    w.println("-(instancetype)initWithXmlElement:(ZKElement *)e;")
    w.println();
    super.writeHeaderProperties(w);
  }

  override protected def writeExtraImpl(w: SourceWriter) {
    val init = if (baseType == null) "init" else "initWithZKXmlDeserializer:d"
    w.println(s"""
            |+(void)load {
            |    [self registerType:self xmlName:@"$xmlName"];
            |}
            |
            |-(instancetype)init {
            |    self = [super init];
            |    return self;
            |}
            |
            |-(instancetype)initWithZKXmlDeserializer:(ZKXmlDeserializer *)d {
            |    self = [super $init];
            |""".stripMargin('|'))

    for (f <- fields) {
      w.println(f.initializer("d"));
    }
    w.println(s"""|    return self;
                |}
                |
                |-(instancetype)initWithXmlElement:(ZKElement *)e {
                |    ZKXmlDeserializer *d = [[ZKXmlDeserializer alloc] initWithXmlElement:e];
                |    return [self initWithZKXmlDeserializer:d];
                |}
                |""".stripMargin('|'))
  }
}

class ZKDescribeField(
    xmlName: String,
    objcName: String,
    xmlNode: Node,
    fields: collection.Seq[ComplexTypeProperty]
) extends OutputComplexTypeInfo(xmlName, objcName, xmlNode, fields, null) {

  override protected def implementNSCopying(): Boolean = { true }

  override protected def writeForwardDecls(w: SourceWriter) {
    w.printClassForwardDecl("ZKDescribeSObject")
    super.writeForwardDecls(w)
  }

  override protected def writeHeaderIVars(w: SourceWriter) {
    w.println("\tZKDescribeSObject *__weak sobject;")
  }

  override protected def writeHeaderProperties(w: SourceWriter) {
    w.println("@property (weak) ZKDescribeSObject *sobject;")
    w.println()
    super.writeHeaderProperties(w)
  }

  override protected def writeImplImports(w: SourceWriter) {
    w.printImport("ZKDescribeSObject.h")
    w.printImport("ZKParser.h")
    super.writeImplImports(w)
  }

  override protected def additionalNSCopyImpl(): String = {
    "c.sobject = self.sobject;"
  }

  override protected def writeImplFileBody(w: SourceWriter) {
    w.println("@synthesize sobject;")
    w.println()
    super.writeImplFileBody(w)
  }
}

class ZKDescribeSObject(
    xmlName: String,
    objcName: String,
    xmlNode: Node,
    fields: collection.Seq[ComplexTypeProperty]
) extends OutputComplexTypeInfo(xmlName, objcName, xmlNode, fields, null) {

  override def headerImportFile(): String = { "ZKDescribeGlobalSObject.h" }
  override def baseClass(): String = { "ZKDescribeGlobalSObject" }

  override protected def writeHeaderIVars(w: SourceWriter) {
    w.println("	NSArray 	 *fieldList;")
    w.println("	NSDictionary *fieldsByName;")
  }

  override protected def writeImplFileBody(w: SourceWriter) {
    w.println("""-(NSArray *)fields {
					|	if (fieldList == nil) {
 					|		NSArray *fa = [self complexTypeArrayFromElements:@"fields" cls:[ZKDescribeField class]];
					|		for (ZKDescribeField *f in fa)
					|			f.sobject = self;
					|		fieldList = fa;
					|	}
					|	return fieldList;
					|}
					|""".stripMargin('|'));

    for (f <- fields.filter(_.propertyName != "fields"))
      w.println(f.readImplBody)
  }
}

class VoidTypeInfo() extends TypeInfo("void", "void", "", false) {
  override def accessor(instanceName: String, elemName: String): String = { "" }

  override def blockTypeName(): String = {
    return "ZKCompleteVoidBlock"
  }
}
