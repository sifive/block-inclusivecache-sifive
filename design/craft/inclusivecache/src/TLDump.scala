package sifive.blocks.inclusivecache

import Chisel._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

trait HasTLDump {

  implicit class TLDump(channel: TLChannel) {
    def dump(params: InclusiveCacheParameters) = channel match {
      case a: TLBundleA =>
        DebugPrint(params, 
          a.channelName + " opcode: %x param: %x size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
          a.opcode, a.param, a.size, a.source, a.address, a.mask, a.data, a.corrupt
        )
      case b: TLBundleB =>
        DebugPrint(params, 
          b.channelName + " opcode: %x param: %x size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
          b.opcode, b.param, b.size, b.source, b.address, b.mask, b.data, b.corrupt
        )
      case c: TLBundleC =>
        DebugPrint(params, 
          c.channelName + " opcode: %x param: %x size: %x source: %d address: %x data: %x corrupt: %b\n",
          c.opcode, c.param, c.size, c.source, c.address, c.data, c.corrupt
        )
      case d: TLBundleD =>
        DebugPrint(params, 
          d.channelName + " opcode: %x param: %x size: %x source: %d sink: %d denied: %b data: %x corrupt: %b\n",
          d.opcode, d.param, d.size, d.source, d.sink, d.denied, d.data, d.corrupt
        )
      case e: TLBundleE =>
        DebugPrint(params, e.channelName + " sink: %d\n", e.sink)
    }
  }
}
