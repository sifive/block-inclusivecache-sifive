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

class SinkDResponse(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val last   = Bool()
  val opcode = UInt(width = 3)
  val param  = UInt(width = 3)
  val source = UInt(width = params.outer.bundle.sourceBits)
  val sink   = UInt(width = params.outer.bundle.sinkBits)
  val denied = Bool()
  def dump() = {
    DebugPrint("SinkDResponse: last: %b opcode: %x param: %x source: %x sink: %x denied: %b\n",
      last, opcode, param, source, sink, denied)
  }
}

class SinkD(params: InclusiveCacheParameters) extends Module with HasTLDump
{
  val io = new Bundle {
    val resp = Valid(new SinkDResponse(params)) // Grant or ReleaseAck
    val d = Decoupled(new TLBundleD(params.outer.bundle)).flip
    // Lookup the set+way from MSHRs
    val source = UInt(width = params.outer.bundle.sourceBits)
    val way    = UInt(width = params.wayBits).flip
    val set    = UInt(width = params.setBits).flip
    // Banked Store port
    val bs_adr = Decoupled(new BankedStoreOuterAddress(params))
    val bs_dat = new BankedStoreOuterPoison(params)
    // WaR hazard
    val grant_req = new SourceDHazard(params)
    val grant_safe = Bool().flip
  }

  when (io.resp.fire()) {
    DebugPrint("sinkD resp ")
    io.resp.bits.dump
  }


  when (io.d.fire()) {
    DebugPrint("outer grant ")
    io.d.bits.dump
  }

  // DebugPrint("sinkD: source: %x set: %x way: %x\n", io.source, io.set, io.way)

  when (io.bs_adr.fire()) {
    DebugPrint("sinkD bs_adr ")
    io.bs_adr.bits.dump
  }

  /*
  DebugPrint("sinkD bs_dat ")
  io.bs_dat.dump

  DebugPrint("sinkD grant_req ")
  io.grant_req.dump

  when (io.grant_safe) {
    DebugPrint("sinkD grant_safe\n")
  }
  */

  // No restrictions on buffer
  val d = params.micro.outerBuf.d(io.d)

  val (first, last, _, beat) = params.outer.count(d)
  val hasData = params.outer.hasData(d.bits)

  // 常见写法，valid时直接用，不valid时，用锁存的值
  io.source := Mux(d.valid, d.bits.source, RegEnable(d.bits.source, d.valid))
  io.grant_req.way := io.way
  io.grant_req.set := io.set

  // Also send Grant(NoData) to BS to ensure correct data ordering
  // 为什么只要在first和last时给valid？
  // 为什么都要not first呢？
  // 看来response只需要给两个，一个是开始，另一个是结束
  io.resp.valid := (first || last) && d.fire()
  // 后面的io.grantSafe又是从哪里来的呢？
  d.ready := io.bs_adr.ready && (!first || io.grant_safe)
  // 总感觉这边有点问题啊？
  // 为啥不是first就要valid呢？
  // 当不是first时，要和不看d是否valid吗？就直接写吗？这也不太对吧？我感觉这个是假设了message都是连续拍的吧？这个不一定能保证的吧？
  io.bs_adr.valid := !first || (d.valid && io.grant_safe)
  params.ccover(d.valid && first && !io.grant_safe, "SINKD_HAZARD", "Prevented Grant data hazard with backpressure")
  params.ccover(io.bs_adr.valid && !io.bs_adr.ready, "SINKD_SRAM_STALL", "Data SRAM busy")

  io.resp.bits.last   := last
  io.resp.bits.opcode := d.bits.opcode
  io.resp.bits.param  := d.bits.param
  io.resp.bits.source := d.bits.source
  io.resp.bits.sink   := d.bits.sink
  io.resp.bits.denied := d.bits.denied

  io.bs_adr.bits.noop := !d.valid || !hasData
  io.bs_adr.bits.way  := io.way
  io.bs_adr.bits.set  := io.set
  io.bs_adr.bits.beat := Mux(d.valid, beat, RegEnable(beat + io.bs_adr.ready.asUInt, d.valid))
  io.bs_adr.bits.mask := ~UInt(0, width = params.outerMaskBits)
  io.bs_dat.data      := d.bits.data

  assert (!(d.valid && d.bits.corrupt && !d.bits.denied), "Data poisoning unsupported")
}
