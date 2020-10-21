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
import freechips.rocketchip.util._

case class ListBufferParameters[T <: Data](gen: T, queues: Int, entries: Int, bypass: Boolean)
{
  val queueBits = log2Up(queues)
  val entryBits = log2Up(entries)
}

class ListBufferPush[T <: Data](params: ListBufferParameters[T]) extends GenericParameterizedBundle(params)
{
  val index = UInt(width = params.queueBits)
  val data  = params.gen.asOutput
}

class ListBuffer[T <: Data](params: ListBufferParameters[T]) extends Module
{
  val io = new Bundle {
    // push is visible on the same cycle; flow queues
    val push  = Decoupled(new ListBufferPush(params)).flip
    val valid = UInt(width = params.queues)
    val pop   = Valid(UInt(width = params.queueBits)).flip
    val data  = params.gen.asOutput
  }

  // queues估计是总共有多少queue
  // entrys是总共有多少entry
  // 然后每个entry就是T
  // 哪几个queue是valid的？
  val valid = RegInit(UInt(0, width=params.queues))
  // 每个queue的head还有tail
  val head  = Mem(params.queues, UInt(width = params.entryBits))
  val tail  = Mem(params.queues, UInt(width = params.entryBits))
  // 但是这边的used, next, data为啥只有entries个呢？有可能是所有mshr共享entries的？
  val used  = RegInit(UInt(0, width=params.entries))
  // 这个怎么有一种链表的感觉？
  val next  = Mem(params.entries, UInt(width = params.entryBits))
  // 这个才是最终的data
  val data  = Mem(params.entries, params.gen)

  // 这边算出哪一项是free的？
  val freeOH = ~(leftOR(~used) << 1) & ~used
  val freeIdx = OHToUInt(freeOH)

  val valid_set = Wire(init = UInt(0, width=params.queues))
  val valid_clr = Wire(init = UInt(0, width=params.queues))
  val used_set  = Wire(init = UInt(0, width=params.entries))
  val used_clr  = Wire(init = UInt(0, width=params.entries))

  // 当前队列的队尾
  val push_tail = tail.read(io.push.bits.index)
  // 这个是说，假如原来这个队列已经是valid的了，那就是拓展队列
  val push_valid = valid(io.push.bits.index)

  // 看是否还有空的entry
  io.push.ready := !used.andR()
  when (io.push.fire()) {
    // 进入哪个queue
    valid_set := UIntToOH(io.push.bits.index, params.queues)
    // 需要把valid的哪一位，set的哪一位给set
    used_set := freeOH
    // 数据局写入
    data.write(freeIdx, io.push.bits.data)
    when (push_valid) {
      next.write(push_tail, freeIdx)
    } .otherwise {
      head.write(io.push.bits.index, freeIdx)
    }
    tail.write(io.push.bits.index, freeIdx)
  }

  val pop_head = head.read(io.pop.bits)
  val pop_valid = valid(io.pop.bits)

  // Bypass push data to the peek port
  io.data := (if (!params.bypass) data.read(pop_head) else Mux(!pop_valid, io.push.bits.data, data.read(pop_head)))
  io.valid := (if (!params.bypass) valid else (valid | valid_set))

  // It is an error to pop something that is not valid
  assert (!io.pop.fire() || (io.valid)(io.pop.bits))

  when (io.pop.fire()) {
    used_clr := UIntToOH(pop_head, params.entries)
    when (pop_head === tail.read(io.pop.bits)) {
      valid_clr := UIntToOH(io.pop.bits, params.queues)
    }
    head.write(io.pop.bits, Mux(io.push.fire() && push_valid && push_tail === pop_head, freeIdx, next.read(pop_head)))
  }

  // Empty bypass changes no state
  when (Bool(!params.bypass) || !io.pop.valid || pop_valid) {
    used  := (used  & ~used_clr)  | used_set
    valid := (valid & ~valid_clr) | valid_set
  }
}
