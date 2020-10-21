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

class SourceBRequest(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val param   = UInt(width = 3)
  val tag     = UInt(width = params.tagBits)
  val set     = UInt(width = params.setBits)
  val clients = UInt(width = params.clientBits)
  def dump() = {
    DebugPrint("SourceBRequest: param: %x tag: %x set: %x clients: %x\n",
      param, tag, set, clients)
  }
}

class SourceB(params: InclusiveCacheParameters) extends Module with HasTLDump
{
  val io = new Bundle {
    val req = Decoupled(new SourceBRequest(params)).flip
    val b = Decoupled(new TLBundleB(params.inner.bundle))
  }

  when (io.b.fire()) {
    DebugPrint("inner probe ")
    io.b.bits.dump
  }
    
  when (io.req.fire()) {
    DebugPrint("sourceB req ")
    io.req.bits.dump
  }

  if (params.firstLevel) {
    // 如果是firstLevel，肯定就不需要发送probe了
    // Tie off unused ports
    io.req.ready := Bool(true)
    io.b.valid := Bool(false)
  } else {
    val remain = RegInit(UInt(0, width=params.clientBits))
    val remain_set = Wire(init = UInt(0, width=params.clientBits))
    val remain_clr = Wire(init = UInt(0, width=params.clientBits))
    remain := (remain | remain_set) & ~remain_clr

    val busy = remain.orR()
    // 哪些是要处理的，如果有busy的话就用remain，否则就直接用clients
    val todo = Mux(busy, remain, io.req.bits.clients)
    // next是随机选中的，下一个要处理的？
    // 问题，为啥处处都是随机的呢？why？
    val next = ~(leftOR(todo) << 1) & todo

    if (params.clientBits > 1) {
      params.ccover(PopCount(remain) > UInt(1), "SOURCEB_MULTI_PROBE", "Had to probe more than one client")
    }

    assert (!io.req.valid || io.req.bits.clients =/= UInt(0))

    io.req.ready := !busy
    // 这里是标注一下，有哪些client需要被probe
    when (io.req.fire()) { remain_set := io.req.bits.clients }

    // No restrictions on the type of buffer used here
    val b = Wire(io.b)
    io.b <> params.micro.innerBuf.b(b)

    b.valid := busy || io.req.valid
    when (b.fire()) { remain_clr := next }
    params.ccover(b.valid && !b.ready, "SOURCEB_STALL", "Backpressured when issuing a probe")

    val tag = Mux(!busy, io.req.bits.tag, RegEnable(io.req.bits.tag, io.req.fire()))
    val set = Mux(!busy, io.req.bits.set, RegEnable(io.req.bits.set, io.req.fire()))
    val param = Mux(!busy, io.req.bits.param, RegEnable(io.req.bits.param, io.req.fire()))

    b.bits.opcode  := TLMessages.Probe
    b.bits.param   := param
    b.bits.size    := UInt(params.offsetBits)
    b.bits.source  := params.clientSource(next)
    b.bits.address := params.expandAddress(tag, set, UInt(0))
    b.bits.mask    := ~UInt(0, width = params.inner.manager.beatBytes)
    b.bits.data    := UInt(0)
  }
}
