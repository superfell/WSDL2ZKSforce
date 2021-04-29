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

class ZKDescribeField(
    xmlName: String,
    objcName: String,
    xmlNode: Node,
    fields: collection.Seq[ComplexTypeProperty]
) extends ComplexTypeInfo(xmlName, objcName, xmlNode, fields, null) {

  override protected def writeForwardDecls(w: SourceWriter) {
    w.printClassForwardDecl("ZKDescribeSObject")
    super.writeForwardDecls(w)
  }

  override protected def writeHeaderIVars(w: SourceWriter) {
    super.writeHeaderIVars(w)
    w.println("\tZKDescribeSObject *__weak sobject;")
  }

  override protected def writeHeaderProperties(w: SourceWriter) {
    w.println("@property (weak) ZKDescribeSObject *sobject;")
    w.println()
    super.writeHeaderProperties(w)
  }

  override protected def writeImplImports(w: SourceWriter) {
    w.printImport("ZKDescribeSObject.h")
    super.writeImplImports(w)
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
    fields: collection.Seq[ComplexTypeProperty],
    baseType: TypeInfo
) extends ComplexTypeInfo(xmlName, objcName, xmlNode, fields, baseType) {

  override protected def writeHeaderIVars(w: SourceWriter) {
    w.println("	NSDictionary *fieldsByName;")
    super.writeHeaderIVars(w)
  }

  override protected def writeWsdlSchema(w: SourceWriter) {
    w.println(
      "// This schema information reflects how ZKSforce maps DescribeSObject"
    )
    w.println(
      "// into being a subclass of DescribeGlobalSObject, rather than"
    )
    w.println(
      "// exactly how it is defined in the WSDL (where they are not related)."
    )
    super.writeWsdlSchema(w)
  }

  override protected def shouldWritePropertyImpls(
      p: ComplexTypeProperty
  ): Boolean = {
    p.propertyName != "fields"
  }

  override protected def writePropertyImpls(w: SourceWriter) {
    w.println("""-(NSArray *)fields {
    |	if (self.fields__v == nil) {
    |		NSArray *fa = [self complexTypeArrayFromElements:@"fields" cls:[ZKDescribeField class]];
    |		for (ZKDescribeField *f in fa) {
    |			f.sobject = self;
    |   }
    |		self.fields__v = fa;
	  |	}
    |	return self.fields__v;
    |}
    |
    |-(void)setFields:(NSArray *)v {
    |    self.fields__v = v;
    |}
    |""".stripMargin('|'))
    super.writePropertyImpls(w)
  }
}
