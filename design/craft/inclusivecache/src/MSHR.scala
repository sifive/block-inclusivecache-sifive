/*
 * 这边默认都设置为true，是在搞啥呢？
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
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.tilelink._
import TLPermissions._
import TLMessages._
import MetaData._

// 这些似乎是对外的一些接口
class ScheduleRequest(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val a = Valid(new SourceARequest(params))
  val b = Valid(new SourceBRequest(params))
  val c = Valid(new SourceCRequest(params))
  val d = Valid(new SourceDRequest(params))
  val e = Valid(new SourceERequest(params))
  val x = Valid(new SourceXRequest(params))
  val dir = Valid(new DirectoryWrite(params))
  val reload = Bool() // get next request via allocate (if any)

  def dump() = {
    when (a.fire()) {
      a.bits.dump()
    }
    when (b.fire()) {
      b.bits.dump()
    }
    when (c.fire()) {
      c.bits.dump()
    }
    when (d.fire()) {
      d.bits.dump()
    }
    when (e.fire()) {
      e.bits.dump()
    }
    when (x.fire()) {
      x.bits.dump()
    }
    when (dir.valid) {
      dir.bits.dump()
    }
    when (reload) {
      DebugPrint("reload\n")
    }
  }
}

// 暴露给scheduler的内部status，主要是用来帮助分配的？
class MSHRStatus(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val set = UInt(width = params.setBits)
  val tag = UInt(width = params.tagBits)
  val way = UInt(width = params.wayBits)
  val blockB = Bool()
  val nestB  = Bool()
  val blockC = Bool()
  val nestC  = Bool()

  def dump() = {
    DebugPrint("MSHRStatus: set: %x tag: %x way: %x blockB: %b nestB: %b blockC: %b nestC: %b\n",
      set, tag, way, blockB, nestB, blockC, nestC)
  }
}

class NestedWriteback(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val set = UInt(width = params.setBits)
  val tag = UInt(width = params.tagBits)
  val b_toN       = Bool() // nested Probes may unhit us
  val b_toB       = Bool() // nested Probes may demote us
  val b_clr_dirty = Bool() // nested Probes clear dirty
  val c_set_dirty = Bool() // nested Releases MAY set dirty
  def dump() = {
    DebugPrint("NestedWriteback: set: %x tag: %x b_toN: %b b_toB: %b b_clr_dirty: %b c_set_dirty: %b\n",
      set, tag, b_toN, b_toB, b_clr_dirty, c_set_dirty)
  }
}

sealed trait CacheState
{
  val code = UInt(CacheState.index)
  CacheState.index = CacheState.index + 1
}

object CacheState
{
  var index = 0
}

// 这边其实就类似enum，每个人的code是自己的index
// 无效态
case object S_INVALID  extends CacheState
// branch
case object S_BRANCH   extends CacheState
// branch with inner branch
case object S_BRANCH_C extends CacheState
// tip without inner branch?
case object S_TIP      extends CacheState
// tip with inner branch
case object S_TIP_C    extends CacheState
// tip with inner branch and dirty
case object S_TIP_CD   extends CacheState
// tip dirty
case object S_TIP_D    extends CacheState
case object S_TRUNK_C  extends CacheState
case object S_TRUNK_CD extends CacheState

class MSHR(params: InclusiveCacheParameters) extends Module
{
  val io = new Bundle {
    // 分配用的口
    val allocate  = Valid(new AllocateRequest(params)).flip // refills MSHR for next cycle
    // directory response
    val directory = Valid(new DirectoryResult(params)).flip // triggers schedule setup
    val status    = Valid(new MSHRStatus(params))
    // 对外发送请求
    val schedule  = Decoupled(new ScheduleRequest(params))
    // 对内接收的请求
    val sinkc     = Valid(new SinkCResponse(params)).flip
    val sinkd     = Valid(new SinkDResponse(params)).flip
    val sinke     = Valid(new SinkEResponse(params)).flip
    val nestedwb  = new NestedWriteback(params).flip
  }

  when (io.allocate.valid) {
    DebugPrint("MSHR allocate: ")
    io.allocate.bits.dump()
  }

  when (io.directory.valid) {
    DebugPrint("MSHR directory: ")
    io.directory.bits.dump()
  }

  when (io.status.valid) {
    DebugPrint("MSHR status: ")
    io.status.bits.dump()
  }

  when (io.schedule.fire()) {
    DebugPrint("MSHR schedule: ")
    io.schedule.bits.dump()
  }

  when (io.sinkc.valid) {
    DebugPrint("MSHR sinkc: ")
    io.sinkc.bits.dump()
  }

  when (io.sinkd.valid) {
    DebugPrint("MSHR sinkd: ")
    io.sinkd.bits.dump()
  }

  when (io.sinke.valid) {
    DebugPrint("MSHR sinke: ")
    io.sinke.bits.dump()
  }

  val request_valid = RegInit(Bool(false))
  val request = Reg(new FullRequest(params))
  val meta_valid = RegInit(Bool(false))
  val meta = Reg(new DirectoryResult(params))

  // Define which states are valid
  // 这是啥意思？
  // branch, trunk, tip到底是啥意思啊？
  when (meta_valid) {
    when (meta.state === INVALID) {
      // 当是invalid时，肯定不能是dirty的，也不能有client拥有这个块儿
      assert (!meta.clients.orR)
      assert (!meta.dirty)
    }
    when (meta.state === BRANCH) {
      // 当是branch时，不能是dirty
      assert (!meta.dirty)
    }
    when (meta.state === TRUNK) {
      assert (meta.clients.orR)
      assert ((meta.clients & (meta.clients - UInt(1))) === UInt(0)) // at most one
    }
    when (meta.state === TIP) {
      // noop
    }
  }

  // Completed transitions (s_ = scheduled), (w_ = waiting)
  // 把inner cache给shoot down，其实就是把要替换的块儿给inner probe
  val s_rprobe         = RegInit(Bool(true)) // B
  val w_rprobeackfirst = RegInit(Bool(true))
  val w_rprobeacklast  = RegInit(Bool(true))

  val s_release        = RegInit(Bool(true)) // CW w_rprobeackfirst
  val w_releaseack     = RegInit(Bool(true))

  // 把我们正在处理的块儿要probe
  val s_pprobe         = RegInit(Bool(true)) // B
  val s_acquire        = RegInit(Bool(true)) // A  s_release, s_pprobe [1]
  val s_flush          = RegInit(Bool(true)) // X  w_releaseack
  val w_grantfirst     = RegInit(Bool(true))
  val w_grantlast      = RegInit(Bool(true))
  val w_grant          = RegInit(Bool(true)) // first | last depending on wormhole
  val w_pprobeackfirst = RegInit(Bool(true))
  val w_pprobeacklast  = RegInit(Bool(true))
  val w_pprobeack      = RegInit(Bool(true)) // first | last depending on wormhole
  val s_probeack       = RegInit(Bool(true)) // C  w_pprobeackfirst (mutually exclusive with next two s_*)
  val s_grantack       = RegInit(Bool(true)) // E  w_grantfirst ... CAN require both outE&inD to service outD
  val s_execute        = RegInit(Bool(true)) // D  w_pprobeack, w_grant
  val w_grantack       = RegInit(Bool(true))
  val s_writeback      = RegInit(Bool(true)) // W  w_*

  // [1]: We cannot issue outer Acquire while holding blockB (=> outA can stall)
  // However, inB and outC are higher priority than outB, so s_release and s_pprobe
  // may be safely issued while blockB. Thus we must NOT try to schedule the
  // potentially stuck s_acquire with either of them (scheduler is all or none).

  // Meta-data that we discover underway
  val sink = Reg(UInt(width = params.outer.bundle.sinkBits))
  val gotT = Reg(Bool())
  val bad_grant = Reg(Bool())
  // 针对哪些client，probe已经done了
  val probes_done = Reg(UInt(width = params.clientBits))
  // 针对哪些client，probe已经toN了
  val probes_toN = Reg(UInt(width = params.clientBits))
  val probes_noT = Reg(Bool())

  // When a nested transaction completes, update our meta data
  when (meta_valid && meta.state =/= INVALID &&
        io.nestedwb.set === request.set && io.nestedwb.tag === meta.tag) {
    when (io.nestedwb.b_clr_dirty) { meta.dirty := Bool(false) }
    when (io.nestedwb.c_set_dirty) { meta.dirty := Bool(true) }
    when (io.nestedwb.b_toB) { meta.state := BRANCH }
    when (io.nestedwb.b_toN) { meta.hit := Bool(false) }
  }

  // Scheduler status
  io.status.valid := request_valid
  io.status.bits.set    := request.set
  io.status.bits.tag    := request.tag
  io.status.bits.way    := meta.way
  io.status.bits.blockB := !meta_valid || ((!w_releaseack || !w_rprobeacklast || !w_pprobeacklast) && !w_grantfirst)
  io.status.bits.nestB  := meta_valid && w_releaseack && w_rprobeacklast && w_pprobeacklast && !w_grantfirst
  // The above rules ensure we will block and not nest an outer probe while still doing our
  // own inner probes. Thus every probe wakes exactly one MSHR.
  io.status.bits.blockC := !meta_valid
  io.status.bits.nestC  := meta_valid && (!w_rprobeackfirst || !w_pprobeackfirst || !w_grantfirst)
  // The w_grantfirst in nestC is necessary to deal with:
  //   acquire waiting for grant, inner release gets queued, outer probe -> inner probe -> deadlock
  // ... this is possible because the release+probe can be for same set, but different tag

  // We can only demand: block, nest, or queue
  assert (!io.status.bits.nestB || !io.status.bits.blockB)
  assert (!io.status.bits.nestC || !io.status.bits.blockC)

  // Scheduler requests
  val no_wait = w_rprobeacklast && w_releaseack && w_grantlast && w_pprobeacklast && w_grantack
  // 发送acquire请求要等待release以及inner probe完成
  io.schedule.bits.a.valid := !s_acquire && s_release && s_pprobe
  // rprobe和pprobe都是啥？
  io.schedule.bits.b.valid := !s_rprobe || !s_pprobe
  io.schedule.bits.c.valid := (!s_release && w_rprobeackfirst) || (!s_probeack && w_pprobeackfirst)
  io.schedule.bits.d.valid := !s_execute && w_pprobeack && w_grant
  io.schedule.bits.e.valid := !s_grantack && w_grantfirst
  io.schedule.bits.x.valid := !s_flush && w_releaseack
  io.schedule.bits.dir.valid := (!s_release && w_rprobeackfirst) || (!s_probeack && w_pprobeackfirst) || (!s_writeback && no_wait)
  io.schedule.bits.reload := no_wait
  io.schedule.valid := io.schedule.bits.a.valid || io.schedule.bits.b.valid || io.schedule.bits.c.valid ||
                       io.schedule.bits.d.valid || io.schedule.bits.e.valid || io.schedule.bits.x.valid ||
                       io.schedule.bits.dir.valid

  // Schedule completions
  when (io.schedule.ready) {
    // 当scheudle ready时，rprobe和probe肯定都完成了
                                    s_rprobe     := Bool(true)
    when (w_rprobeackfirst)       { s_release    := Bool(true) }
                                    s_pprobe     := Bool(true)
    when (s_release && s_pprobe)  { s_acquire    := Bool(true) }
    when (w_releaseack)           { s_flush      := Bool(true) }
    when (w_pprobeackfirst)       { s_probeack   := Bool(true) }
    when (w_grantfirst)           { s_grantack   := Bool(true) }
    when (w_pprobeack && w_grant) { s_execute    := Bool(true) }
    when (no_wait)                { s_writeback  := Bool(true) }
    // Await the next operation
    when (no_wait) {
      request_valid := Bool(false)
      meta_valid := Bool(false)
    }
  }

  // Resulting meta-data
  val final_meta_writeback = Wire(init = meta)

  val req_clientBit = params.clientBit(request.source)
  val req_needT = needT(request.opcode, request.param)
  val req_acquire = request.opcode === AcquireBlock || request.opcode === AcquirePerm
  val meta_no_clients = !meta.clients.orR
  val req_promoteT = req_acquire && Mux(meta.hit, meta_no_clients && meta.state === TIP, gotT)

  when (request.prio(2) && Bool(!params.firstLevel)) { // always a hit
    final_meta_writeback.dirty   := meta.dirty || request.opcode(0)
    final_meta_writeback.state   := Mux(request.param =/= TtoT && meta.state === TRUNK, TIP, meta.state)
    final_meta_writeback.clients := meta.clients & ~Mux(isToN(request.param), req_clientBit, UInt(0))
    final_meta_writeback.hit     := Bool(true) // chained requests are hits
  } .elsewhen (request.control && Bool(params.control)) { // request.prio(0)
    when (meta.hit) {
      final_meta_writeback.dirty   := Bool(false)
      final_meta_writeback.state   := INVALID
      final_meta_writeback.clients := meta.clients & ~probes_toN
    }
    final_meta_writeback.hit := Bool(false)
  } .otherwise {
    final_meta_writeback.dirty := (meta.hit && meta.dirty) || !request.opcode(2)
    final_meta_writeback.state := Mux(req_needT,
                                    Mux(req_acquire, TRUNK, TIP),
                                    Mux(!meta.hit, Mux(gotT, Mux(req_acquire, TRUNK, TIP), BRANCH),
                                      MuxLookup(meta.state, UInt(0, width=2), Seq(
                                        INVALID -> BRANCH,
                                        BRANCH  -> BRANCH,
                                        TRUNK   -> TIP,
                                        TIP     -> Mux(meta_no_clients && req_acquire, TRUNK, TIP)))))
    final_meta_writeback.clients := Mux(meta.hit, meta.clients & ~probes_toN, UInt(0)) |
                                    Mux(req_acquire, req_clientBit, UInt(0))
    final_meta_writeback.tag := request.tag
    final_meta_writeback.hit := Bool(true)
  }

  when (bad_grant) {
    when (meta.hit) {
      // upgrade failed (B -> T)
      assert (!meta_valid || meta.state === BRANCH)
      final_meta_writeback.hit     := Bool(true)
      final_meta_writeback.dirty   := Bool(false)
      final_meta_writeback.state   := BRANCH
      final_meta_writeback.clients := meta.clients & ~probes_toN
    } .otherwise {
      // failed N -> (T or B)
      final_meta_writeback.hit     := Bool(false)
      final_meta_writeback.dirty   := Bool(false)
      final_meta_writeback.state   := INVALID
      final_meta_writeback.clients := UInt(0)
    }
  }

  val invalid = Wire(new DirectoryEntry(params))
  invalid.dirty   := Bool(false)
  invalid.state   := INVALID
  invalid.clients := UInt(0)
  invalid.tag     := UInt(0)

  // Just because a client says BtoT, by the time we process the request he may be N.
  // Therefore, we must consult our own meta-data state to confirm he owns the line still.
  val honour_BtoT = meta.hit && (meta.clients & req_clientBit).orR

  // The client asking us to act is proof they don't have permissions.
  // 就是对于请求的client，是否要probe它们自己
  // 这个主要是为了避免重复的probe
  val excluded_client = Mux(meta.hit && request.prio(0) && skipProbeN(request.opcode), req_clientBit, UInt(0))
  io.schedule.bits.a.bits.tag     := request.tag
  io.schedule.bits.a.bits.set     := request.set
  io.schedule.bits.a.bits.param   := Mux(req_needT, Mux(meta.hit, BtoT, NtoT), NtoB)
  io.schedule.bits.a.bits.block   := request.size =/= UInt(log2Ceil(params.cache.blockBytes)) ||
                                     !(request.opcode === PutFullData || request.opcode === AcquirePerm)
  io.schedule.bits.a.bits.source  := UInt(0)
  // 如果是rprobe，那就是要替换，那就是直接toN
  // request.prio(1)是1，是啥意思？
  // 不过req_needT是显然的，如果要变成T，显然就是直接把上面toN
  // 如果是不需要T，就是直接读，那就是toB
  io.schedule.bits.b.bits.param   := Mux(!s_rprobe, toN, Mux(request.prio(1), request.param, Mux(req_needT, toN, toB)))
  io.schedule.bits.b.bits.tag     := Mux(!s_rprobe, meta.tag, request.tag)
  io.schedule.bits.b.bits.set     := request.set
  io.schedule.bits.b.bits.clients := meta.clients & ~excluded_client
  io.schedule.bits.c.bits.opcode  := Mux(meta.dirty, ReleaseData, Release)
  io.schedule.bits.c.bits.param   := Mux(meta.state === BRANCH, BtoN, TtoN)
  io.schedule.bits.c.bits.source  := UInt(0)
  io.schedule.bits.c.bits.tag     := meta.tag
  io.schedule.bits.c.bits.set     := request.set
  io.schedule.bits.c.bits.way     := meta.way
  io.schedule.bits.c.bits.dirty   := meta.dirty
  io.schedule.bits.d.bits         := request
  io.schedule.bits.d.bits.param   := Mux(!req_acquire, request.param,
                                       MuxLookup(request.param, Wire(request.param), Seq(
                                         NtoB -> Mux(req_promoteT, NtoT, NtoB),
                                         BtoT -> Mux(honour_BtoT,  BtoT, NtoT),
                                         NtoT -> NtoT)))
  io.schedule.bits.d.bits.sink    := UInt(0)
  io.schedule.bits.d.bits.way     := meta.way
  io.schedule.bits.d.bits.bad     := bad_grant
  io.schedule.bits.e.bits.sink    := sink
  io.schedule.bits.x.bits.fail    := Bool(false)
  io.schedule.bits.dir.bits.set   := request.set
  io.schedule.bits.dir.bits.way   := meta.way
  io.schedule.bits.dir.bits.data  := Mux(!s_release || !s_probeack, invalid, Wire(new DirectoryEntry(params), init = final_meta_writeback))

  // Coverage of state transitions
  // 这边是一系列的assert，主要是为了覆盖cache state的转换？
  // 似乎是为了尽可能覆盖cache状态的转换？
  def cacheState(entry: DirectoryEntry, hit: Bool) = {
    val out = Wire(UInt())
    // 是否有client拥有这个块儿
    val c = entry.clients.orR
    // 是否是dirty的
    val d = entry.dirty
    switch (entry.state) {
      is (BRANCH)  { out := Mux(c, S_BRANCH_C.code, S_BRANCH.code) }
      // 我们是trunk，意味着我们有master是tip
      // 值得足以的是trunk状态也可以是dirty的
      // 最新的数据可能在我们这里，也可能在client那里
      // 首先trunk肯定是带C的，意味着权限是在client那里
      // 也可能带d，也可能不带d
      // 带d的话，它的数据可能没有client那么新。
      is (TRUNK)   { out := Mux(d, S_TRUNK_CD.code, S_TRUNK_C.code) }
      // tip可以不带C
      // tip如果有C，就是上面有branch
      // tip如果有d，就是有脏数据没有写回
      is (TIP)     { out := Mux(c, Mux(d, S_TIP_CD.code, S_TIP_C.code), Mux(d, S_TIP_D.code, S_TIP.code)) }
      is (INVALID) { out := S_INVALID.code }
    }
    // 如果没有hit，那肯定要变成invalid了
    when (!hit) { out := S_INVALID.code }
    out
  }

  val p = !params.lastLevel  // can be probed
  val c = !params.firstLevel // can be acquired
  // 这两个看不懂是在干啥?
  val m = params.inner.client.clients.exists(!_.supportsProbe)   // can be written (or read)
  val r = params.outer.manager.managers.exists(!_.alwaysGrantsT) // read-only devices exist
  val f = params.control     // flush control register exists
  val cfg = (p, c, m, r, f)
  val b = r || p // can reach branch state (via probe downgrade or read-only device)

  // The cache must be used for something or we would not be here
  require(c || m)

  // evict是啥？
  // 如果是hit，那就说明是tag匹配的话，没那么
  val evict = cacheState(meta, !meta.hit)
  val before = cacheState(meta, meta.hit)
  // 在最后write back的时候，肯定是hit的，那这个是根据meta，标记一下state
  val after  = cacheState(final_meta_writeback, Bool(true))

  def eviction(from: CacheState, cover: Boolean)(implicit sourceInfo: SourceInfo) {
    if (cover) {
      params.ccover(evict === from.code, s"MSHR_${from}_EVICT", s"State transition from ${from} to evicted ${cfg}")
    } else {
      assert(!(evict === from.code), s"State transition from ${from} to evicted should be impossible ${cfg}")
    }
    if (cover && f) {
      params.ccover(before === from.code, s"MSHR_${from}_FLUSH", s"State transition from ${from} to flushed ${cfg}")
    } else {
      assert(!(before === from.code), s"State transition from ${from} to flushed should be impossible ${cfg}")
    }
  }

  def transition(from: CacheState, to: CacheState, cover: Boolean)(implicit sourceInfo: SourceInfo) {
    if (cover) {
      params.ccover(before === from.code && after === to.code, s"MSHR_${from}_${to}", s"State transition from ${from} to ${to} ${cfg}")
    } else {
      assert(!(before === from.code && after === to.code), s"State transition from ${from} to ${to} should be impossible ${cfg}")
    }
  }

  when ((!s_release && w_rprobeackfirst) && io.schedule.ready) {
    eviction(S_BRANCH,    b)      // MMIO read to read-only device
    eviction(S_BRANCH_C,  b && c) // you need children to become C
    eviction(S_TIP,       true)   // MMIO read || clean release can lead to this state
    eviction(S_TIP_C,     c)      // needs two clients || client + mmio || downgrading client
    eviction(S_TIP_CD,    c)      // needs two clients || client + mmio || downgrading client
    eviction(S_TIP_D,     true)   // MMIO write || dirty release lead here
    eviction(S_TRUNK_C,   c)      // acquire for write
    eviction(S_TRUNK_CD,  c)      // dirty release then reacquire
  }

  when ((!s_writeback && no_wait) && io.schedule.ready) {
    transition(S_INVALID,  S_BRANCH,   b && m) // only MMIO can bring us to BRANCH state
    transition(S_INVALID,  S_BRANCH_C, b && c) // C state is only possible if there are inner caches
    transition(S_INVALID,  S_TIP,      m)      // MMIO read
    transition(S_INVALID,  S_TIP_C,    false)  // we would go S_TRUNK_C instead
    transition(S_INVALID,  S_TIP_CD,   false)  // acquire does not cause dirty immediately
    transition(S_INVALID,  S_TIP_D,    m)      // MMIO write
    transition(S_INVALID,  S_TRUNK_C,  c)      // acquire
    transition(S_INVALID,  S_TRUNK_CD, false)  // acquire does not cause dirty immediately

    transition(S_BRANCH,   S_INVALID,  b && p) // probe can do this (flushes run as evictions)
    transition(S_BRANCH,   S_BRANCH_C, b && c) // acquire
    transition(S_BRANCH,   S_TIP,      b && m) // prefetch write
    transition(S_BRANCH,   S_TIP_C,    false)  // we would go S_TRUNK_C instead
    transition(S_BRANCH,   S_TIP_CD,   false)  // acquire does not cause dirty immediately
    transition(S_BRANCH,   S_TIP_D,    b && m) // MMIO write
    transition(S_BRANCH,   S_TRUNK_C,  b && c) // acquire
    transition(S_BRANCH,   S_TRUNK_CD, false)  // acquire does not cause dirty immediately

    transition(S_BRANCH_C, S_INVALID,  b && c && p)
    transition(S_BRANCH_C, S_BRANCH,   b && c)      // clean release (optional)
    transition(S_BRANCH_C, S_TIP,      b && c && m) // prefetch write
    transition(S_BRANCH_C, S_TIP_C,    false)       // we would go S_TRUNK_C instead
    transition(S_BRANCH_C, S_TIP_D,    b && c && m) // MMIO write
    transition(S_BRANCH_C, S_TIP_CD,   false)       // going dirty means we must shoot down clients
    transition(S_BRANCH_C, S_TRUNK_C,  b && c)      // acquire
    transition(S_BRANCH_C, S_TRUNK_CD, false)       // acquire does not cause dirty immediately

    transition(S_TIP,      S_INVALID,  p)
    transition(S_TIP,      S_BRANCH,   p)      // losing TIP only possible via probe
    transition(S_TIP,      S_BRANCH_C, false)  // we would go S_TRUNK_C instead
    transition(S_TIP,      S_TIP_C,    false)  // we would go S_TRUNK_C instead
    transition(S_TIP,      S_TIP_D,    m)      // direct dirty only via MMIO write
    transition(S_TIP,      S_TIP_CD,   false)  // acquire does not make us dirty immediately
    transition(S_TIP,      S_TRUNK_C,  c)      // acquire
    transition(S_TIP,      S_TRUNK_CD, false)  // acquire does not make us dirty immediately

    transition(S_TIP_C,    S_INVALID,  c && p)
    transition(S_TIP_C,    S_BRANCH,   c && p) // losing TIP only possible via probe
    transition(S_TIP_C,    S_BRANCH_C, c && p) // losing TIP only possible via probe
    transition(S_TIP_C,    S_TIP,      c)      // probed while MMIO read || clean release (optional)
    transition(S_TIP_C,    S_TIP_D,    c && m) // direct dirty only via MMIO write
    transition(S_TIP_C,    S_TIP_CD,   false)  // going dirty means we must shoot down clients
    transition(S_TIP_C,    S_TRUNK_C,  c)      // acquire
    transition(S_TIP_C,    S_TRUNK_CD, false)  // acquire does not make us immediately dirty

    transition(S_TIP_D,    S_INVALID,  p)
    transition(S_TIP_D,    S_BRANCH,   p)      // losing D is only possible via probe
    transition(S_TIP_D,    S_BRANCH_C, p && c) // probed while acquire shared
    transition(S_TIP_D,    S_TIP,      p)      // probed while MMIO read || outer probe.toT (optional)
    transition(S_TIP_D,    S_TIP_C,    false)  // we would go S_TRUNK_C instead
    transition(S_TIP_D,    S_TIP_CD,   false)  // we would go S_TRUNK_CD instead
    transition(S_TIP_D,    S_TRUNK_C,  p && c) // probed while acquired
    transition(S_TIP_D,    S_TRUNK_CD, c)      // acquire

    transition(S_TIP_CD,   S_INVALID,  c && p)
    transition(S_TIP_CD,   S_BRANCH,   c && p) // losing D is only possible via probe
    transition(S_TIP_CD,   S_BRANCH_C, c && p) // losing D is only possible via probe
    transition(S_TIP_CD,   S_TIP,      c && p) // probed while MMIO read || outer probe.toT (optional)
    transition(S_TIP_CD,   S_TIP_C,    false)  // we would go S_TRUNK_C instead
    transition(S_TIP_CD,   S_TIP_D,    c)      // MMIO write || clean release (optional)
    transition(S_TIP_CD,   S_TRUNK_C,  c && p) // probed while acquire
    transition(S_TIP_CD,   S_TRUNK_CD, c)      // acquire

    transition(S_TRUNK_C,  S_INVALID,  c && p)
    transition(S_TRUNK_C,  S_BRANCH,   c && p) // losing TIP only possible via probe
    transition(S_TRUNK_C,  S_BRANCH_C, c && p) // losing TIP only possible via probe
    transition(S_TRUNK_C,  S_TIP,      c)      // MMIO read || clean release (optional)
    transition(S_TRUNK_C,  S_TIP_C,    c)      // bounce shared
    transition(S_TRUNK_C,  S_TIP_D,    c)      // dirty release
    transition(S_TRUNK_C,  S_TIP_CD,   c)      // dirty bounce shared
    transition(S_TRUNK_C,  S_TRUNK_CD, c)      // dirty bounce

    transition(S_TRUNK_CD, S_INVALID,  c && p)
    transition(S_TRUNK_CD, S_BRANCH,   c && p) // losing D only possible via probe
    transition(S_TRUNK_CD, S_BRANCH_C, c && p) // losing D only possible via probe
    transition(S_TRUNK_CD, S_TIP,      c && p) // probed while MMIO read || outer probe.toT (optional)
    transition(S_TRUNK_CD, S_TIP_C,    false)  // we would go S_TRUNK_C instead
    transition(S_TRUNK_CD, S_TIP_D,    c)      // dirty release
    transition(S_TRUNK_CD, S_TIP_CD,   c)      // bounce shared
    transition(S_TRUNK_CD, S_TRUNK_C,  c && p) // probed while acquire
  }

  // Handle response messages
  // 这次回来的这个probAck是哪次的？
  val probe_bit = params.clientBit(io.sinkc.bits.source)
  // probes_done是标志已经完成了哪些client的probeAck
  // 最后一个显然就是收到了之后，就完成了所有的probe
  val last_probe = (probes_done | probe_bit) === (meta.clients & ~excluded_client)
  // 是不是变成toN的probeAck
  val probe_toN = isToN(io.sinkc.bits.param)
  if (!params.firstLevel) when (io.sinkc.valid) {
    params.ccover( probe_toN && io.schedule.bits.b.bits.param === toB, "MSHR_PROBE_FULL", "Client downgraded to N when asked only to do B")
    params.ccover(!probe_toN && io.schedule.bits.b.bits.param === toB, "MSHR_PROBE_HALF", "Client downgraded to B when asked only to do B")
    // Caution: the probe matches us only in set.
    // We would never allow an outer probe to nest until both w_[rp]probeack complete, so
    // it is safe to just unguardedly update the probe FSM.
    // 收到一个probeAck后，就更新probesdDone
    probes_done := probes_done | probe_bit
    probes_toN := probes_toN | Mux(probe_toN, probe_bit, UInt(0))
    // 这个probes notT是啥意思？
    probes_noT := probes_noT || io.sinkc.bits.param =/= TtoT
    // 怎么感觉还是标false，知道最后一个last probe到位？
    w_rprobeackfirst := w_rprobeackfirst || last_probe
    // 这个为啥还要sinkC的last标记一下？why？
    w_rprobeacklast := w_rprobeacklast || (last_probe && io.sinkc.bits.last)
    w_pprobeackfirst := w_pprobeackfirst || last_probe
    w_pprobeacklast := w_pprobeacklast || (last_probe && io.sinkc.bits.last)
    // 啥意思？
    // Allow wormhole routing from sinkC if the first request beat has offset 0
    val set_pprobeack = last_probe && (io.sinkc.bits.last || request.offset === UInt(0))
    w_pprobeack := w_pprobeack || set_pprobeack
    params.ccover(!set_pprobeack && w_rprobeackfirst, "MSHR_PROBE_SERIAL", "Sequential routing of probe response data")
    params.ccover( set_pprobeack && w_rprobeackfirst, "MSHR_PROBE_WORMHOLE", "Wormhole routing of probe response data")
    // However, meta-data updates need to be done more cautiously
    // 啥意思？
    when (meta.state =/= INVALID && io.sinkc.bits.tag === meta.tag && io.sinkc.bits.data) { meta.dirty := Bool(true) } // !!!
  }
  when (io.sinkd.valid) {
    when (io.sinkd.bits.opcode === Grant || io.sinkd.bits.opcode === GrantData) {
      sink := io.sinkd.bits.sink
      w_grantfirst := Bool(true)
      w_grantlast := io.sinkd.bits.last
      // Record if we need to prevent taking ownership
      bad_grant := io.sinkd.bits.denied
      // Allow wormhole routing for requests whose first beat has offset 0
      w_grant := request.offset === UInt(0) || io.sinkd.bits.last
      params.ccover(io.sinkd.bits.opcode === GrantData && request.offset === UInt(0), "MSHR_GRANT_WORMHOLE", "Wormhole routing of grant response data")
      params.ccover(io.sinkd.bits.opcode === GrantData && request.offset =/= UInt(0), "MSHR_GRANT_SERIAL", "Sequential routing of grant response data")
      gotT := io.sinkd.bits.param === toT
    }
    .elsewhen (io.sinkd.bits.opcode === ReleaseAck) {
      w_releaseack := Bool(true)
    }
  }
  when (io.sinke.valid) {
    w_grantack := Bool(true)
  }

  // Bootstrap new requests
  val allocate_as_full = Wire(new FullRequest(params), init = io.allocate.bits)
  // 如果是allocate并且是repeat的，那么就用自己本地更新的最新的metadata
  // 否则就用directory里面读出来的
  val new_meta = Mux(io.allocate.valid && io.allocate.bits.repeat, final_meta_writeback, io.directory.bits)
  val new_request = Mux(io.allocate.valid, allocate_as_full, request)
  // 是否需要trunk权限
  val new_needT = needT(new_request.opcode, new_request.param)
  // new_clientBit这个显然就是自己了
  val new_clientBit = params.clientBit(new_request.source)
  // skipProbe应该是说自己是否要被probe？
  val new_skipProbe = Mux(skipProbeN(new_request.opcode), new_clientBit, UInt(0))

  val prior = cacheState(final_meta_writeback, Bool(true))
  def bypass(from: CacheState, cover: Boolean)(implicit sourceInfo: SourceInfo) {
    if (cover) {
      params.ccover(prior === from.code, s"MSHR_${from}_BYPASS", s"State bypass transition from ${from} ${cfg}")
    } else {
      assert(!(prior === from.code), s"State bypass from ${from} should be impossible ${cfg}")
    }
  }

  when (io.allocate.valid && io.allocate.bits.repeat) {
    bypass(S_INVALID,   f || p) // Can lose permissions (probe/flush)
    bypass(S_BRANCH,    b)      // MMIO read to read-only device
    bypass(S_BRANCH_C,  b && c) // you need children to become C
    bypass(S_TIP,       true)   // MMIO read || clean release can lead to this state
    bypass(S_TIP_C,     c)      // needs two clients || client + mmio || downgrading client
    bypass(S_TIP_CD,    c)      // needs two clients || client + mmio || downgrading client
    bypass(S_TIP_D,     true)   // MMIO write || dirty release lead here
    bypass(S_TRUNK_C,   c)      // acquire for write
    bypass(S_TRUNK_CD,  c)      // dirty release then reacquire
  }

  when (io.allocate.valid) {
    // allocate时要么是request not valid
    // 另外的条件是啥的？
    assert (!request_valid || (no_wait && io.schedule.fire()))
    request_valid := Bool(true)
    request := io.allocate.bits
  }

  // Create execution plan
  // meta的来源有两个，假如这个mshr就有，那就用这个，否则就用从directory里面读出来的
  // 估计是等meta到位了，然后就开始搞execution plan了
  // 那看来这里的的execution plan，总共就这些了
  // 所有的行为估计都是根据这些变量来的？
  // 我感觉可以打印一下来看看执行流程？
  when (io.directory.valid || (io.allocate.valid && io.allocate.bits.repeat)) {
    meta_valid := Bool(true)
    meta := new_meta
    probes_done := UInt(0)
    probes_toN := UInt(0)
    probes_noT := Bool(false)
    gotT := Bool(false)
    bad_grant := Bool(false)

    // These should already be either true or turning true
    // We clear them here explicitly to simplify the mux tree
    // 这些变量都是干啥的？
    // 这边默认都设置为true，是在搞啥呢？
    s_rprobe         := Bool(true)
    w_rprobeackfirst := Bool(true)
    w_rprobeacklast  := Bool(true)
    s_release        := Bool(true)
    w_releaseack     := Bool(true)
    s_pprobe         := Bool(true)
    s_acquire        := Bool(true)
    s_flush          := Bool(true)
    w_grantfirst     := Bool(true)
    w_grantlast      := Bool(true)
    w_grant          := Bool(true)
    w_pprobeackfirst := Bool(true)
    w_pprobeacklast  := Bool(true)
    w_pprobeack      := Bool(true)
    s_probeack       := Bool(true)
    s_grantack       := Bool(true)
    s_execute        := Bool(true)
    w_grantack       := Bool(true)
    s_writeback      := Bool(true)

    // For C channel requests (ie: Release[Data])
    // 这边进来的需要处理的请求只有ACE
    // AC都是client主动发起的请求
    // 如果我们要处理对下的probe，我们还要能处理下面来的probe请求
    // 由于是InclusiveCache，所以来的请求肯定hit，所以不需要考虑release的问题。只需要考虑要不要修改本地的meta的问题
    // 从C来的请求带的param，有TtoB，TtoN, BtoN
    // 其实还有一个问题，如果是作为L2的话，它不能只发Acquire，不然就无法搞shared
    // C通道进来肯定hit，所以它只需要考虑meta需不需要修改。
    // 数据写入banked store，是在SinkC那里处理的。
    // 至于那里是怎么拿到way的，我还是不太懂。估计是给req时，分配mshr时，就把directory读好了？
    when (new_request.prio(2) && Bool(!params.firstLevel)) {
      s_execute := Bool(false)
      // Do we need to go dirty?
      // ProbeAck     4 100
      // ProbeAckData 5 101
      // Release      6 110
      // ReleaseData  7 111
      // opcode(0)就是选中了ProbeAckData & ReleaseData
      // 问题：C通道来的请求为啥会有dirty的呢？
      // 如果要写回数据，并且不是dirty的，需要写meta
      when (new_request.opcode(0) && !new_meta.dirty) {
        s_writeback := Bool(false)
      }
      // Does our state change?
      // 新的reques同时toB
      // 为什么toB就一定要搭配TRUNK?
      // 如果是toB，那么就可能是从T变过来的，那假如我们是trunk的话，我们可能就要变成tip
      when (isToB(new_request.param) && new_meta.state === TRUNK) {
        s_writeback := Bool(false)
      }
      // Do our clients change?
      // client bit标记的是上面是不是有数据，似乎没有标记上面具体是什么权限。
      // 以为也似乎不需要区分上面是什么权限。
      // 假如是L2 block是shared，上面肯定也是shared
      // 假如L2是exclusive，上面肯定是shared
      // 假如L2是trunk，那么上面肯定是tip。上面不可能是branch，因为branch只能长在tip上
      // 如果把当前client的bit给与一下之后就是零了，说明当前来的这个client完全不在我们记录中？
      // 那说明我们不应该收到这个请求的？那不应该assert吗？
      // 返回假如client不在directory中，那就不用修改了
      // 假如在，那就要改meta。
      when (isToN(new_request.param) && (new_meta.clients & new_clientBit) =/= UInt(0)) {
        s_writeback := Bool(false)
      }
      assert (new_meta.hit)
    }
    // For X channel requests (ie: flush)
    // 这个是用来支持cache flush指令的
    .elsewhen (new_request.control && Bool(params.control)) { // new_request.prio(0)
      s_flush := Bool(false)
      // Do we need to actually do something?
      when (new_meta.hit) {
        s_release := Bool(false)
        w_releaseack := Bool(false)
        // Do we need to shoot-down inner caches?
        when (Bool(!params.firstLevel) && (new_meta.clients =/= UInt(0))) {
          s_rprobe := Bool(false)
          w_rprobeackfirst := Bool(false)
          w_rprobeacklast := Bool(false)
        }
      }
    }
    // For B channel requests (ie: ProbeBlock/ProbePerm)
    // 假如不是last level cache，就要处理probe请求
    .elsewhen (new_request.prio(1) && Bool(!params.lastLevel)) {
      // Do we need to actually do something?
      s_probeack := Bool(false)
      when (new_meta.hit) {
        // Do we need to shoot-down inner caches?
        when (Bool(!params.firstLevel) && (new_meta.clients =/= UInt(0))) {
          s_pprobe := Bool(false)
          w_pprobeackfirst := Bool(false)
          w_pprobeacklast := Bool(false)
        }
      }
    }
    // For A channel requests
    .otherwise { // new_request.prio(0) && !new_request.control
      s_execute := Bool(false)
      // 我感觉下面这个流程分得也非常清楚
      // 1. 先判断是不是要把这个块儿给evict掉，相应的，对于这个块儿的probe也在这里判断
      // 2. 再判断是不是要acquire
      // 3. 再判断是不是要probe自己的client，因为probe别人的client，已经在1完成了
      // 4. 再判断对于这个请求，是不是要回复一个GrantAck
      // 5. 最后再判断，我们这个需不需要变dirty，改meta
      // Do we need an eviction?
      // 没有hit，并且不是invalid，那这个是什么意思呢？
      // 说明我们命中的这个块儿肯定是别的块儿
      // 这个块儿需要被release
      // release这里就不需要标注write back了，因为早晚会被write back吗？
      when (!new_meta.hit && new_meta.state =/= INVALID) {
        s_release := Bool(false)
        w_releaseack := Bool(false)
        // Do we need to shoot-down inner caches?
        // first level不需要把内部的给拿掉
        // meta data的clients应该就是标记了哪些clients有这些块儿，这个没有记录这些clients具体是什么权限。
        // 我们要把要eviction的块儿给probe
        when (Bool(!params.firstLevel) & (new_meta.clients =/= UInt(0))) {
          s_rprobe := Bool(false)
          w_rprobeackfirst := Bool(false)
          w_rprobeacklast := Bool(false)
        }
      }
      // 我们没有hit，那就是没有这个块儿，那肯定要acquire
      // 另外假如有这个块儿，那就要检查权限了
      // Do we need an acquire?
      // 假如是另外的状态，例如trunk或者tip，权限肯定在我们的子树内，我们通过probe就可以拿到了
      // 关键是搞懂这些状态变量的意思？
      // not hit有两种：
      // 1. nothing肯定是not hit
      // 2. 权限不够
      // 既然是权限不够，那肯定就是要走一下A，D，E的流程了
      // 我们这个对于get，或许是应该拿shared状态
      // 毕竟按照我们现在的设计，只要到我们这儿的请求，我们肯定都会get的
      // 所以应该就是肯定是acquire block，不过权限方面，对于get，我们可以考虑拿shared，而不是每个块儿都拿exclusive
      when (!new_meta.hit || (new_meta.state === BRANCH && new_needT)) {
        s_acquire := Bool(false)
        w_grantfirst := Bool(false)
        w_grantlast := Bool(false)
        w_grant := Bool(false)
        s_grantack := Bool(false)
        s_writeback := Bool(false)
      }
      // Do we need a probe?
      // 如果我们需要tip权限，并且client有拿着块儿的
      // 或者我们是trunk权限，我们现在根本就不是最新的块儿，根本就无法处理，也要把内部再probe一下
      // 我们要把这个块儿给probe
      when (Bool(!params.firstLevel) && (new_meta.hit &&
            (new_needT || new_meta.state === TRUNK) &&
            (new_meta.clients & ~new_skipProbe) =/= UInt(0))) {
              // s_pprobe和s_pprobe是干啥的呢？
        s_pprobe := Bool(false)
        w_pprobeackfirst := Bool(false)
        w_pprobeacklast := Bool(false)
        w_pprobeack := Bool(false)
        s_writeback := Bool(false)
      }
      // Do we need a grantack?
      // 是否要等待内部发grantAck
      // uncache的请求是只要A和D就可以了
      // E是需要等待grantAck的
      when (new_request.opcode === AcquireBlock || new_request.opcode === AcquirePerm) {
        w_grantack := Bool(false)
        s_writeback := Bool(false)
      }
      // Becomes dirty?
      when (!new_request.opcode(2) && new_meta.hit && !new_meta.dirty) {
        s_writeback := Bool(false)
      }
    }
  }
}
