package sifive.blocks.inclusivecache

import chisel3._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

object GTimer {
  def apply() = {
    val c = RegInit(0.U(64.W))
    c := c + 1.U
    c
  }
}


object DebugPrint {
  def apply(params: InclusiveCacheParameters, fmt: String, data: Bits*): Any =
    apply(params, Printable.pack(fmt, data:_*))

  def apply(params: InclusiveCacheParameters, pable: Printable): Any = {
    val commonInfo = p"[time=${GTimer()}] ${params.cacheName}: "
    printf(commonInfo + pable)
  }
}
