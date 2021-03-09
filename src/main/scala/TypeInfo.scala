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
  def propertyFlags(): String = {
    if (isPointer) "strong,nonatomic" else "assign,nonatomic"
  }

  def propertyLengthForPaddingCalc(): Int = {
    fullTypeName.length
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
    else if (objcName == "int64_t") "addInt64Element"
    else "addElement"
  }

  // type name for use with blocks
  def blockTypeName(): String = {
    return "ZKComplete" + objcName.substring(2) + "Block"
  }

  def classDepth(): Int = 0;
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

class VoidTypeInfo() extends TypeInfo("void", "void", "", false) {
  override def accessor(instanceName: String, elemName: String): String = { "" }

  override def blockTypeName(): String = {
    return "ZKCompleteVoidBlock"
  }
}
