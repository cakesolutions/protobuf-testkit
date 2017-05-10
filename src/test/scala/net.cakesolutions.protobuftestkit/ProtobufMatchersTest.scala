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

import org.scalatest.FlatSpec

class ProtobufMatchersTest extends FlatSpec with ProtobufMatchers {

  it should "accept trivial compatibility" in {
    Envelope() should be (compatibleWith(Envelope))
  }

}
