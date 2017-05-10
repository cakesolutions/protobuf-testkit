/*
BSD 3-Clause License

Copyright (c) 2017, Cake Solutions Limited
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

* Neither the name of the copyright holder nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.
 */
package net.cakesolutions.protobuftestkit

import com.google.protobuf.{ByteString, Descriptors}
import com.google.protobuf.Descriptors.FieldDescriptor
import com.trueaccord.scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}
import org.scalacheck.{Arbitrary, Gen}

/**
  * Defines generators for Protobuf-based messages
  */
object ProtobufGen {

  lazy val genByteString: Gen[ByteString] = {
    Gen.containerOf[Array, Byte](Arbitrary.arbByte.arbitrary).map(ByteString.copyFrom)
  }

  /**
    * Returns a generator for the ScalaPB-based message defined by its ``companion``. The companion is typically
    * generated during the ``protobuf:protobuf-generate`` sbt task, which also typically runs during the ``package``
    * task.
    *
    * @param companion the companion for the message type ``M``
    * @tparam M the message type
    * @return generator for arbitrary messages of type ``M``
    */
  def message[M <: GeneratedMessage with Message[M]](companion: GeneratedMessageCompanion[M]): Gen[M] = {
    import collection.JavaConversions._

    /**
      * Maps the list of ``FieldDescriptor``s into a list of pairs of the descriptor with the matching
      * ``Gen[_]`` for that field.
      * @param fields the field descriptors
      * @return the fd and matching generator
      */
    def generatorsFromFields(fields: List[FieldDescriptor]): List[(FieldDescriptor, Gen[Any])] = {
      fields.map { field ⇒
        import Descriptors.FieldDescriptor._

        val fieldGen: Gen[Any] = field.getType match {
          case Type.BOOL ⇒ Arbitrary.arbBool.arbitrary
          case Type.BYTES ⇒ genByteString
          case Type.DOUBLE ⇒ Arbitrary.arbDouble.arbitrary
          case Type.ENUM ⇒ Gen.oneOf(companion.enumCompanionForField(field).javaDescriptor.getValues.toList)
          case Type.FIXED32 ⇒ Arbitrary.arbDouble.arbitrary
          case Type.FIXED64 ⇒ Arbitrary.arbDouble.arbitrary
          case Type.FLOAT ⇒ Arbitrary.arbFloat.arbitrary
          case Type.GROUP ⇒ ???
          case Type.INT32 ⇒ Arbitrary.arbInt.arbitrary //.withFilter(_ >= 0)
          case Type.INT64 ⇒ Arbitrary.arbBigInt.arbitrary //.withFilter(_ >= 0)
          case Type.MESSAGE ⇒ existentialMessage(companion.messageCompanionForField(field))
          case Type.SFIXED32 ⇒ Arbitrary.arbDouble.arbitrary
          case Type.SFIXED64 ⇒ Arbitrary.arbDouble.arbitrary
          case Type.SINT32 ⇒ Arbitrary.arbInt.arbitrary
          case Type.SINT64 ⇒ Arbitrary.arbInt.arbitrary
          case Type.STRING ⇒ Arbitrary.arbString.arbitrary
          case Type.UINT32 ⇒ Arbitrary.arbInt.arbitrary //.withFilter(_ >= 0)
          case Type.UINT64 ⇒ Arbitrary.arbBigInt.arbitrary //.withFilter(_ >= 0)
        }

        if (field.isRepeated) {
          (field, Gen.listOf(fieldGen))
        } else {
          (field, fieldGen)
        }
      }
    }

    def flatten(generators: List[(FieldDescriptor, Gen[Any])]): Gen[Map[FieldDescriptor, Any]] = {
      assert(generators.nonEmpty, "The list of generators must not be empty.")

      val (ffd, ffg) = generators.head
      val firstFieldGenerator = ffg.map(x ⇒ Map(ffd → x))
      val remainingFieldGenerators = generators.tail

      remainingFieldGenerators.foldLeft(firstFieldGenerator) {
        case (result, (fd, fg)) ⇒ result.flatMap(m ⇒ fg.map(x ⇒ Map(fd → x) ++ m))
      }
    }

    /**
      * Constructs a generator for instances generated from generated fields from the ``companion``.
      * @param companion the companion
      * @return the generator of messages constructed using the companion
      */
    def existentialMessage(companion: GeneratedMessageCompanion[_]): Gen[_] = {
      val oneOfFields = companion.javaDescriptor.getOneofs.toList
      val fields = companion.javaDescriptor.getFields.toList

      // first, construct generators for plain fields: i.e. those that are not algebraic
      val plainGenerator = flatten(generatorsFromFields(fields.filterNot(oneOfFields.flatMap(_.getFields.toList).contains)))
      // next, construct generators for algebraic fields by constructing a generator that selects one field from each one-of group,
      // then constructing a generator for that field and folding the generators
      val oneOfGenerators = oneOfFields.map { oneOf ⇒
        Gen.oneOf(oneOf.getFields.toList).flatMap { fd ⇒
          val (_, generator) = generatorsFromFields(List(fd)).head
          generator.map(x ⇒ Map(fd → x))
        }
      }

      // finally, combine the generators for the one-of fields with the plain fields
      val combinedGenerators = oneOfGenerators match {
        case Nil    ⇒ plainGenerator
        case (h::t) ⇒
          val combinedOneOfGenerator = t.foldLeft(h)((result, gen) ⇒ result.flatMap(m ⇒ gen.map(x ⇒ m ++ x)))
          combinedOneOfGenerator.flatMap(o ⇒ plainGenerator.map(x ⇒ o ++ x))
      }

      // use the companion to construct an instance from the field map
      combinedGenerators.map(companion.fromFieldsMap)
    }

    // the innards of ScalaPB do not allow to express properly compile-time type checking: it would be only
    // possible to check the top-level type, but possible inner messages would be left "untyped", so for the
    // sake of commonality, we use ``existentialMessage(...).map(_.asInstanceOf[M])`` here.
    existentialMessage(companion).map(_.asInstanceOf[M])
  }

}
