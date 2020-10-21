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
import freechips.rocketchip.tilelink._

class SinkB(params: InclusiveCacheParameters) extends Module with HasTLDump
{
  val io = new Bundle {
    val req = Decoupled(new FullRequest(params))
    val b = Decoupled(new TLBundleB(params.outer.bundle)).flip
  }

  when (io.req.fire()) {
    DebugPrint("sinkB req ")
    io.req.bits.dump
  }

  when (io.b.fire()) {
    DebugPrint("inner probe ")
    io.b.bits.dump
  }
    
  val b = params.micro.innerBuf.c(io.b)

  val (tag, set, offset) = params.parseAddress(b.bits.address)

  b.ready := io.req.ready
  io.req.valid := b.valid
  params.ccover(b.valid && !b.ready, "SINKB_STALL", "Backpressure when accepting a probe message")

  io.req.bits.prio   := Vec(UInt(2, width=3).asBools)
  io.req.bits.control:= Bool(false)
  io.req.bits.opcode := b.bits.opcode
  io.req.bits.param  := b.bits.param
  io.req.bits.size   := b.bits.size
  io.req.bits.source := b.bits.source
  io.req.bits.offset := offset
  io.req.bits.set    := set
  io.req.bits.tag    := tag
}
