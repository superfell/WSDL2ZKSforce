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

// For types that are mapped from ComplexTypes (aka Classes)
class ComplexTypeInfo(
    xmlName: String,
    objcName: String,
    xmlNode: Node,
    val fields: collection.Seq[ComplexTypeProperty],
    baseType: TypeInfo
) extends TypeInfo(xmlName, objcName, "", true) {

  override def classDepth(): Int = {
    if (baseType == null) 1 else baseType.classDepth() + 1
  }

  override def accessor(instanceName: String, elemName: String): String = {
    s"""[$instanceName complexTypeArrayFromElements:@"$elemName" cls:[${objcName} class]].lastObject"""
  }

  protected def headerImportFile(): String = {
    if (baseType == null) "ZKXMLSerializable.h" else baseType.objcName + ".h"
  }

  protected def baseClass(): String = {
    if (baseType == null) "ZKXmlDeserializer"
    else baseType.objcName
  }

  protected def protocols(): Seq[String] = {
    if (baseType == null) Seq("ZKXMLSerializable") else Seq()
  }

  def writeHeaderFile() {
    val hfile = new File(new File("output/generated"), objcName + ".h")
    hfile.getParentFile().mkdirs()
    val h = new SourceWriter(hfile)
    h.printLicenseComment()
    writeHeaderImports(h)
    h.println()
    writeForwardDecls(h)
    h.println("/*")
    val pp = new PrettyPrinter(809, 2)
    h.println(pp.format(xmlNode))
    h.println("*/")
    val protos = protocols()
    val protoStr =
      if (protos.size == 0) "" else "<" + protos.mkString(",") + ">"
    h.println(s"@interface $objcName : ${baseClass} $protoStr {");
    writeHeaderIVars(h)
    h.println("}")
    writeHeaderProperties(h)
    h.println("@end")
    h.close()
  }

  protected def writeHeaderImports(w: SourceWriter) {
    w.printImport(headerImportFile)
    w.printImport("ZKComplexTypeFieldInfo.h")
    w.printImport("ZKXmlDeserializer.h")
    w.printImport("ZKParser.h")
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

  protected def padMembersTo(): Int = {
    if (fields.length == 0) 0
    else
      fields.map(_.propType.propertyLengthForPaddingCalc()).max + 1
  }

  protected def writeHeaderIVars(w: SourceWriter) {
    val psMgr = new PropertySetManger(classDepth(), 0, fields.size)
    w.println(s"\t${psMgr.decl}")
  }

  protected def writeHeaderProperties(w: SourceWriter) {
    w.println("+(ZKComplexTypeInfo *)wsdlSchema;")
    w.println();
    val padTo = padMembersTo()
    fields.foreach(f => w.println(f.propertyDecl(padTo)))
  }

  def writeImplFile() {
    val ifile = new File(new File("output/generated"), objcName + ".m")
    val w = new SourceWriter(ifile)
    w.printLicenseComment()
    w.printImport(objcName + ".h")
    writeImplImports(w)
    w.println()
    writeCategories(w)
    w.println()
    w.println(s"@implementation $objcName")
    w.println()
    writeImplFileBody(w)
    writeDescriptionMaybe(w)
    w.println("@end")
    w.close()
  }

  protected def writeImplImports(w: SourceWriter) {
    w.printImport("ZKEnvelope.h")
    w.printImports(fields.map(_.propType))
  }

  protected def writeCategories(w: SourceWriter) {
    w.println(s"@interface $objcName()")
    val padTo = padMembersTo()
    for (f <- fields) {
      w.println(f.propertyValDecl(padTo))
    }
    w.println("@end")
  }

  protected def writeImplFileBody(w: SourceWriter) {
    writeRegistration(w)
    writeWsdlSchema(w)
    writePropertyImpls(w)
    writeSerializeTo(w)
  }

  protected def writeSerializeTo(w: SourceWriter) {
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

  protected def writeRegistration(w: SourceWriter) {
    w.println(s"""
            |+(void)load {
            |    [self registerType:self xmlName:@"$xmlName"];
            |}
            |""".stripMargin('|'))
    w.println()
  }

  protected def writeWsdlSchema(w: SourceWriter) {
    w.println(s"""+(ZKComplexTypeInfo *)wsdlSchema {
    |   static ZKComplexTypeInfo *wsdlSchema;
    |   static dispatch_once_t onceToken;
    |   dispatch_once(&onceToken, ^{
    |       wsdlSchema = [[ZKComplexTypeInfo alloc] initWithType:@"$xmlName" parent:${if (
      baseType == null
    ) "nil"
    else "[" + baseType.objcName + " class]"}
    |                    fields:@[""".stripMargin('|'))
    for (f <- fields) {
      w.println(
        s"""                        [[ZKComplexTypeFieldInfo alloc] initWithElementName:@"${f.elementName}" propertyName:@"${f.propertyName}" optional:${if (
          f.optional
        ) "YES"
        else "NO"} nillable:${if (f.nillable) "YES" else "NO"}],"""
      )
    }
    w.println(s"""
    |                    ]];
    |   });
    |   return wsdlSchema;
    |}
    """.stripMargin('|'))
  }

  protected def writePropertyImpls(w: SourceWriter) {
    for ((f, fIdx) <- fields.zipWithIndex) {
      if (!shouldWritePropertyImpls(f)) {
        return
      }
      // property getter
      val psMgr = new PropertySetManger(classDepth(), fIdx, fields.size)

      val accessor = f.propType.accessor("self", f.elementName)
      w.println(s"""
        |-(${f.propType.fullTypeName})${f.propertyName} {
        |    if ((${psMgr.variable} & ${psMgr.bitMask}) == 0) {
        |        self.${f.propertyName}__v = ${accessor};
        |        ${psMgr.variable} |= ${psMgr.bitMask}; 
        |    }
        |    return self.${f.propertyName}__v;
        |}
        """.stripMargin('|'))
      // property setter
      w.println(s"""
        |-(void)${f.propertySetterName}:(${f.propType.fullTypeName})v {
        |    self.${f.propertyName}__v = v;
        |    ${psMgr.variable} |= ${psMgr.bitMask}; 
        |}
        """.stripMargin('|'))
    }
  }

  protected def shouldWritePropertyImpls(p: ComplexTypeProperty): Boolean = true

  def writeDescriptionMaybe(w: SourceWriter) {
    // If we have a single field and its an array, generate a description impl for it. This is
    // useful for the numerous string list types (e.g. RelationshipReferenceTo)
    if (fields.size == 1) {
      val f = fields.apply(0)
      f.propType match {
        case a: ArrayTypeInfo => {
          a.componentType match {
            case a: ArrayTypeInfo   => {}
            case c: ComplexTypeInfo => {}
            case s: TypeInfo => {
              w.println(s"""
                        |-(NSString*)description {
                        |   return [self.${f.propertyName} componentsJoinedByString:@","];
                        |}
                        """.stripMargin('|'))
            }
            case _ => {}
          }
        }
        case _ => {}
      }
    }
  }
}

class PropertySetManger(depth: Int, idx: Int, size: Int) {

  def ivarName(): String = {
    if (depth > 1) s"fields__set${depth}" else "fields__set"
  }

  def decl(): String = {
    val len = (size + 64) / 64
    val typ = if (size <= 16) { "UInt16" }
    else if (size <= 32) { "UInt32" }
    else { "UInt64" }
    s"${typ}   ${ivarName}[$len];"
  }

  def variable: String = {
    s"${ivarName}[${idx / 64}]"
  }

  def bitMask: String = {
    val bit = idx % 64;
    "0x%x".format(1L << bit)
  }
}
