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
import freechips.rocketchip.util._

class PutBufferAEntry(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val data = UInt(width = params.inner.bundle.dataBits)
  val mask = UInt(width = params.inner.bundle.dataBits/8)
  val corrupt = Bool()
  def dump() = {
    DebugPrint("PutBufferAEntry: data: %x mask: %x corrupt: %b\n",
      data, mask, corrupt)
  }
}

class PutBufferPop(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val index = UInt(width = params.putBits)
  val last = Bool()
  def dump() = {
    DebugPrint("PutBufferAEntry: index: %x last: %b\n",
      index, last)
  }
}

class SinkA(params: InclusiveCacheParameters) extends Module with HasTLDump
{
  val io = new Bundle {
    val req = Decoupled(new FullRequest(params))
    val a = Decoupled(new TLBundleA(params.inner.bundle)).flip
    // for use by SourceD:
    val pb_pop  = Decoupled(new PutBufferPop(params)).flip
    val pb_beat = new PutBufferAEntry(params)
  }

  when (io.req.fire()) {
    DebugPrint("sinkA req ")
    io.req.bits.dump
  }

  when (io.a.fire()) {
    DebugPrint("inner acquire ")
    io.a.bits.dump
  }
    
  when (io.pb_pop.fire()){
    DebugPrint("sinkA pb_pop ")
    io.pb_pop.bits.dump
  }
    
  /*
  DebugPrint("sinkA pb_beat ")
  io.pb_beat.dump
  */

  // No restrictions on the type of buffer
  val a = params.micro.innerBuf.a(io.a)

  // 这边的put buffer entry，是用来存放写的数据的
  // 总共可以存放有putList个请求
  val putbuffer = Module(new ListBuffer(ListBufferParameters(new PutBufferAEntry(params), params.putLists, params.putBeats, false)))
  // 这个是标识了哪些list在working
  val lists = RegInit(UInt(0, width = params.putLists))

  // lists_set轨迹是这一周期分配哪个list出去
  // lists_clr是这一周期有哪个lists被释放出去
  val lists_set = Wire(init = UInt(0, width = params.putLists))
  val lists_clr = Wire(init = UInt(0, width = params.putLists))
  lists := (lists | lists_set) & ~lists_clr

  val free = !lists.andR()
  // 还是有点不知道这个left OR是用来干啥的？
  // 需要专家来解读一下
  val freeOH = ~(leftOR(~lists) << 1) & ~lists
  val freeIdx = OHToUInt(freeOH)

  val first = params.inner.first(a)
  val hasData = params.inner.hasData(a.bits)

  // We need to split the A input to three places:
  //   If it is the first beat, it must go to req
  //   If it has Data, it must go to the putbuffer
  //   If it has Data AND is the first beat, it must claim a list

  // 这边也说了A请求的注意点
  // 这个好像是这样子的，reqBlock是指cache内现在不能接受req
  // 但是这个又要加上first，因为只有first才需要发送到cache内，来建立req
  val req_block = first && !io.req.ready
  // buf block则是要保存数据，这个只要有数据，就得保存
  val buf_block = hasData && !putbuffer.io.push.ready
  // set block是建立保存的list，这个只有对于第一拍需要建立list
  val set_block = hasData && first && !free

  params.ccover(a.valid && req_block, "SINKA_REQ_STALL", "No MSHR available to sink request")
  params.ccover(a.valid && buf_block, "SINKA_BUF_STALL", "No space in putbuffer for beat")
  params.ccover(a.valid && set_block, "SINKA_SET_STALL", "No space in putbuffer for request")

  a.ready := !req_block && !buf_block && !set_block
  io.req.valid := a.valid && first && !buf_block && !set_block
  putbuffer.io.push.valid := a.valid && hasData && !req_block && !set_block
  // 这边为什么没有查set block呢，是不是少了啥？
  when (a.valid && first && hasData && !req_block && !buf_block) { lists_set := freeOH }

  val (tag, set, offset) = params.parseAddress(a.bits.address)
  // put是对于这个请求，要放到哪个list？
  val put = Mux(first, freeIdx, RegEnable(freeIdx, first))

  // acquire的prio是001
  io.req.bits.prio   := Vec(UInt(1, width=3).asBools)
  io.req.bits.control:= Bool(false)
  io.req.bits.opcode := a.bits.opcode
  io.req.bits.param  := a.bits.param
  io.req.bits.size   := a.bits.size
  io.req.bits.source := a.bits.source
  io.req.bits.offset := offset
  io.req.bits.set    := set
  io.req.bits.tag    := tag
  io.req.bits.put    := put

  putbuffer.io.push.bits.index := put
  putbuffer.io.push.bits.data.data    := a.bits.data
  putbuffer.io.push.bits.data.mask    := a.bits.mask
  putbuffer.io.push.bits.data.corrupt := a.bits.corrupt

  // Grant access to pop the data
  // 当数据收集全了后，就要pop，这个数据是怎么pop出来的呢？是子写进去的吗？肯定不是，肯定还得有替换算法的。
  putbuffer.io.pop.bits := io.pb_pop.bits.index
  putbuffer.io.pop.valid := io.pb_pop.fire()
  io.pb_pop.ready := putbuffer.io.valid(io.pb_pop.bits.index)
  io.pb_beat := putbuffer.io.data

  when (io.pb_pop.fire() && io.pb_pop.bits.last) {
    lists_clr := UIntToOH(io.pb_pop.bits.index, params.putLists)
  }
}
