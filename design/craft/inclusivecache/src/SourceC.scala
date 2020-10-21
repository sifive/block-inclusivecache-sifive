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

class SourceCRequest(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val opcode = UInt(width = 3)
  val param  = UInt(width = 3)
  val source = UInt(width = params.outer.bundle.sourceBits)
  val tag    = UInt(width = params.tagBits)
  val set    = UInt(width = params.setBits)
  val way    = UInt(width = params.wayBits)
  val dirty  = Bool()
  def dump() = {
    DebugPrint("SourceCRequest: opcode: %x param: %x source: %x tag: %x set: %x way: %x dirty: %b\n",
      opcode, param, source, tag, set, way, dirty)
  }
}

class SourceC(params: InclusiveCacheParameters) extends Module with HasTLDump
{
  val io = new Bundle {
    val req = Decoupled(new SourceCRequest(params)).flip
    val c = Decoupled(new TLBundleC(params.outer.bundle))
    // BankedStore port
    val bs_adr = Decoupled(new BankedStoreOuterAddress(params))
    val bs_dat = new BankedStoreOuterDecoded(params).flip
    // RaW hazard
    val evict_req = new SourceDHazard(params)
    val evict_safe = Bool().flip
  }

  when (io.req.fire()) {
    DebugPrint("SourceC req ")
    io.req.bits.dump
  }

  when (io.c.fire()) {
    DebugPrint("outer release ")
    io.c.bits.dump
  }
    
  when (io.bs_adr.fire()){
    DebugPrint("SourceC bs_adr ")
    io.bs_adr.bits.dump
  }
    
  /*
  DebugPrint("SourceC bs_dat ")
  io.bs_dat.dump
    
  DebugPrint("SourceC evict_req ")
  io.evict_req.dump
    
  when (io.evict_safe){
    DebugPrint("SourceC evict_safe\n")
  }
  */
    
  // We ignore the depth and pipe is useless here (we have to provision for worst-case=stall)
  require (!params.micro.outerBuf.c.pipe)

  val beatBytes = params.outer.manager.beatBytes
  val beats = params.cache.blockBytes / beatBytes
  val flow = params.micro.outerBuf.c.flow
  // 这边创建了一个queue，然后空间是拍数加3？
  // 估计是把结果放到queue里？why？
  val queue = Module(new Queue(io.c.bits, beats + 3 + (if (flow) 0 else 1), flow = flow))

  // queue.io.count is far too slow
  val fillBits = log2Up(beats + 4)
  // 估计fill就是我们自己维护的一个counter？
  val fill = RegInit(UInt(0, width = fillBits))
  val room = RegInit(Bool(true))
  // 如果enq和deq都fire了，那计数器肯定就不用变了
  when (queue.io.enq.fire() =/= queue.io.deq.fire()) {
    // enq那就加1，全1就是-1
    fill := fill + Mux(queue.io.enq.fire(), UInt(1), ~UInt(0, width = fillBits))
    // room是empty的意思吗？
    // 估计是empty的意思？那fill 1还可以理解，fill 2就不太好理解了啊？
    room := fill === UInt(0) || ((fill === UInt(1) || fill === UInt(2)) && !queue.io.enq.fire())
  }
  assert (room === queue.io.count <= UInt(1))

  // room是个什么鬼东西？

  val busy = RegInit(Bool(false))
  // beat是用来计数总共到了第多少个beat的
  // 所以beat到底是睁着书
  val beat = RegInit(UInt(0, width = params.outerBeatBits))
  // last是全1，就是到了最后一个？
  val last = beat.andR
  // 这个就是req，从第一拍到最后一拍就可以用
  val req  = Mux(!busy, io.req.bits, RegEnable(io.req.bits, !busy && io.req.valid))
  // 是否需要读取banked store
  val want_data = busy || (io.req.valid && room && io.req.bits.dirty)

  // 只要不是不是busy，并且还有room，就OK
  // 似乎假如请求进来不是dirty的，那根本就不会有任何动作吗？
  io.req.ready := !busy && room

  io.evict_req.set := req.set
  io.evict_req.way := req.way

  // 只要beat还有1，那就要接着读
  // 那可能一开始是传进来evict safe然后就开始
  // 等开始后，后面的就是根据beat来？
  io.bs_adr.valid := (beat.orR || io.evict_safe) && want_data
  io.bs_adr.bits.noop := Bool(false)
  io.bs_adr.bits.way  := req.way
  io.bs_adr.bits.set  := req.set
  io.bs_adr.bits.beat := beat
  io.bs_adr.bits.mask := ~UInt(0, width = params.outerMaskBits)

  params.ccover(io.req.valid && io.req.bits.dirty && room && !io.evict_safe, "SOURCEC_HAZARD", "Prevented Eviction data hazard with backpressure")
  params.ccover(io.bs_adr.valid && !io.bs_adr.ready, "SOURCEC_SRAM_STALL", "Data SRAM busy")

  // 如果是要写回的块儿，才会dirty
  when (io.req.valid && room && io.req.bits.dirty) { busy := Bool(true) }
  // 等到最后一个时，busy才会变成false
  when (io.bs_adr.fire()) {
    when (last) { busy := Bool(false) }
    beat := beat + UInt(1)
  }

  // 似乎假如不want data的话，似乎也能进C，就是直接给权限之类的了
  val s2_latch = Mux(want_data, io.bs_adr.fire(), io.req.fire())
  val s2_valid = RegNext(s2_latch)
  val s2_req = RegEnable(req, s2_latch)
  val s2_beat = RegEnable(beat, s2_latch)
  val s2_last = RegEnable(last, s2_latch)

  val s3_latch = s2_valid
  val s3_valid = RegNext(s3_latch)
  val s3_req = RegEnable(s2_req, s3_latch)
  val s3_beat = RegEnable(s2_beat, s3_latch)
  val s3_last = RegEnable(s2_last, s3_latch)

  val c = Wire(io.c)
  c.valid        := s3_valid
  c.bits.opcode  := s3_req.opcode
  c.bits.param   := s3_req.param
  c.bits.size    := UInt(params.offsetBits)
  c.bits.source  := s3_req.source
  c.bits.address := params.expandAddress(s3_req.tag, s3_req.set, UInt(0))
  c.bits.data    := io.bs_dat.data
  c.bits.corrupt := Bool(false)

  // We never accept at the front-end unless we're sure things will fit
  assert(!c.valid || c.ready)
  params.ccover(!c.ready, "SOURCEC_QUEUE_FULL", "Eviction queue fully utilized")

  // 所以这边似乎是把数据先读出来，构造出C请求，再把C请求放进queue里面去！

  queue.io.enq <> c
  io.c <> queue.io.deq
}
