/*
 * Copyright 2019 SiFive, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You should have received a copy of LICENSE.Apache2 along with
 * this software. If not, you may obtain a copy at
 *
 *    https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sifive.blocks.inclusivecache

import Chisel._

class QueuedRequest(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val prio   = Vec(3, Bool()) // A=001, B=010, C=100
  val control= Bool() // control command
  val opcode = UInt(width = 3)
  val param  = UInt(width = 3)
  val size   = UInt(width = params.inner.bundle.sizeBits)
  val source = UInt(width = params.inner.bundle.sourceBits)
  val tag    = UInt(width = params.tagBits)
  val offset = UInt(width = params.offsetBits)
  val put    = UInt(width = params.putBits)
}

class FullRequest(params: InclusiveCacheParameters) extends QueuedRequest(params)
{
  val set = UInt(width = params.setBits)
}

class AllocateRequest(params: InclusiveCacheParameters) extends FullRequest(params)
{
  val repeat = Bool() // set is the same
}
