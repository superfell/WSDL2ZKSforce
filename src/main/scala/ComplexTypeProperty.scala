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

object ComplexTypeProperty {
  val reservedWords = Set("inline", "description", "hash")
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
    if (
      ComplexTypeProperty.reservedWords.contains(n)
      || n.toLowerCase().startsWith("new")
      || n.toLowerCase().startsWith("copy")
    ) "a_" + n
    else n
  }

  def propertySetterName(): String = {
    "set" + propertyName.capitalize
  }

  def propertyDecl(padTypeTo: Int): String = {
    val f = propType.propertyFlags
    val comment = propType.propertyDeclComment
    val td = typeDef(padTypeTo)
    s"@property ($f) $td; $comment"
  }

  def propertyValDecl(padTypeTo: Int): String = {
    val f = propType.propertyFlags
    val td = typeDef(padTypeTo) + "__v"
    s"@property ($f) $td;"
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
