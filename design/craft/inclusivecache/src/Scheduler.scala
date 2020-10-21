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
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

class Scheduler(params: InclusiveCacheParameters) extends Module
{
  val io = new Bundle {
    val in = TLBundle(params.inner.bundle).flip
    val out = TLBundle(params.outer.bundle)
    // Way permissions
    val ways = Vec(params.allClients, UInt(width = params.cache.ways)).flip
    val divs = Vec(params.allClients, UInt(width = InclusiveCacheParameters.lfsrBits + 1)).flip
    // Control port
    val req = Decoupled(new SinkXRequest(params)).flip
    val resp = Decoupled(new SourceXRequest(params))
  }

  val sourceA = Module(new SourceA(params))
  val sourceB = Module(new SourceB(params))
  val sourceC = Module(new SourceC(params))
  val sourceD = Module(new SourceD(params))
  val sourceE = Module(new SourceE(params))
  val sourceX = Module(new SourceX(params))

  io.out.a <> sourceA.io.a
  io.out.c <> sourceC.io.c
  io.out.e <> sourceE.io.e
  io.in.b <> sourceB.io.b
  io.in.d <> sourceD.io.d
  io.resp <> sourceX.io.x

  val sinkA = Module(new SinkA(params))
  val sinkB = Module(new SinkB(params))
  val sinkC = Module(new SinkC(params))
  val sinkD = Module(new SinkD(params))
  val sinkE = Module(new SinkE(params))
  val sinkX = Module(new SinkX(params))

  sinkA.io.a <> io.in.a
  sinkB.io.b <> io.out.b
  sinkC.io.c <> io.in.c
  sinkE.io.e <> io.in.e
  sinkD.io.d <> io.out.d
  sinkX.io.x <> io.req

  // io.out.b.ready := Bool(true) // disconnected

  val directory = Module(new Directory(params))
  val bankedStore = Module(new BankedStore(params))
  // 我感觉这个就是用来放secondary请求的？就是说有请求就从总线上拿下来放这里？
  // 它这儿的queue的数量是3 * mshrs，这个是有啥讲究吗？
  // secondary比mshr要多？
  val requests = Module(new ListBuffer(ListBufferParameters(new QueuedRequest(params), 3*params.mshrs, params.secondary, false)))
  val mshrs = Seq.fill(params.mshrs) { Module(new MSHR(params)) }
  // 这是是个啥鬼东西啊？
  // init是得到列表中除了最后一项以外的其他所有项
  // abc mshrs是除了倒数两项之外的所有项
  val abc_mshrs = mshrs.init.init
  // bc mshr是倒数第二项
  val bc_mshr = mshrs.init.last
  // c mshr是倒数第一项
  val c_mshr = mshrs.last
  val nestedwb = Wire(new NestedWriteback(params))

  // 这边的valid怎么就直接过来了呢？
  // Deliver messages from Sinks to MSHRs
  // 按照tilelink手册的描述，A和C通道的请求是按照地址来的，所以这边直接就按照set来分了？可能不是这个原因
  // 然后sinkD和E是是直接拿i做的
  mshrs.zipWithIndex.foreach { case (m, i) =>
    m.io.sinkc.valid := sinkC.io.resp.valid && sinkC.io.resp.bits.set === m.io.status.bits.set
    m.io.sinkd.valid := sinkD.io.resp.valid && sinkD.io.resp.bits.source === UInt(i)
    m.io.sinke.valid := sinkE.io.resp.valid && sinkE.io.resp.bits.sink   === UInt(i)
    m.io.sinkc.bits := sinkC.io.resp.bits
    m.io.sinkd.bits := sinkD.io.resp.bits
    m.io.sinke.bits := sinkE.io.resp.bits
    m.io.nestedwb := nestedwb
  }

  // 这边的mshr好像分成了抢占mshr还有普通mshr？
  // If the pre-emption BC or C MSHR have a matching set, the normal MSHR must be blocked
  // 看bc和c是否和abc match
  // 这边的stall只考虑后面的是否需要stall住前面的，而并没有考虑说前面的是不是valid的，是否真的被stall住了
  val mshr_stall_abc = abc_mshrs.map { m =>
    (bc_mshr.io.status.valid && m.io.status.bits.set === bc_mshr.io.status.bits.set) ||
    ( c_mshr.io.status.valid && m.io.status.bits.set ===  c_mshr.io.status.bits.set)
  }
  // c是否stall bc
  val mshr_stall_bc =
    c_mshr.io.status.valid && bc_mshr.io.status.bits.set === c_mshr.io.status.bits.set
  val mshr_stall_c = Bool(false)
  val mshr_stall = mshr_stall_abc :+ mshr_stall_bc :+ mshr_stall_c

  // 总的来说，似乎是普通mshr最低，第二高，c最高


  // abc是否要被stall
  val stall_abc = (mshr_stall_abc zip abc_mshrs) map { case (s, m) => s && m.io.status.valid }
  if (!params.lastLevel || !params.firstLevel)
    params.ccover(stall_abc.reduce(_||_), "SCHEDULER_ABC_INTERLOCK", "ABC MSHR interlocked due to pre-emption")
  if (!params.lastLevel)
    params.ccover(mshr_stall_bc && bc_mshr.io.status.valid, "SCHEDULER_BC_INTERLOCK", "BC MSHR interlocked due to pre-emption")

  // Consider scheduling an MSHR only if all the resources it requires are available
  // Cat过的都需要reverse一下
  // 这个是说schedule valid，并且没有被stall住，并且所有的发请求的口都ready？
  // 只要是这个的，肯定就是可以发送request的？
  val mshr_request = Cat((mshrs zip mshr_stall).map { case (m, s) =>
    m.io.schedule.valid && !s &&
      (sourceA.io.req.ready || !m.io.schedule.bits.a.valid) &&
      (sourceB.io.req.ready || !m.io.schedule.bits.b.valid) &&
      (sourceC.io.req.ready || !m.io.schedule.bits.c.valid) &&
      (sourceD.io.req.ready || !m.io.schedule.bits.d.valid) &&
      (sourceE.io.req.ready || !m.io.schedule.bits.e.valid) &&
      (sourceX.io.req.ready || !m.io.schedule.bits.x.valid) &&
      (directory.io.write.ready || !m.io.schedule.bits.dir.valid)
  }.reverse)

  // Round-robin arbitration of MSHRs
  // 这边是mshr的请求发出去，但是只能发送一个？why？这么多channel不是应该能并行地发送吗？
  val robin_filter = RegInit(UInt(0, width = params.mshrs))
  val robin_request = Cat(mshr_request, mshr_request & robin_filter)
  val mshr_selectOH2 = ~(leftOR(robin_request) << 1) & robin_request
  val mshr_selectOH = mshr_selectOH2(2*params.mshrs-1, params.mshrs) | mshr_selectOH2(params.mshrs-1, 0)
  // 这个是这波儿选出的request
  val mshr_select = OHToUInt(mshr_selectOH)
  val schedule = Mux1H(mshr_selectOH, mshrs.map(_.io.schedule.bits))
  val scheduleTag = Mux1H(mshr_selectOH, mshrs.map(_.io.status.bits.tag))
  val scheduleSet = Mux1H(mshr_selectOH, mshrs.map(_.io.status.bits.set))

  // When an MSHR wins the schedule, it has lowest priority next time
  when (mshr_request.orR()) { robin_filter := ~rightOR(mshr_selectOH) }

  // Fill in which MSHR sends the request
  schedule.a.bits.source := mshr_select
  // 这个是啥意思？ReleaseData和PorobeAckData有啥差别吗？
  // Release[Data]是6 7， ProbeAck是4 5
  // 所以release时id是选mshr
  // 如果是probeAck，就是零，这是为啥呢？
  schedule.c.bits.source := Mux(schedule.c.bits.opcode(1), mshr_select, UInt(0)) // only set for Release[Data] not ProbeAck[Data]
  schedule.d.bits.sink   := mshr_select

  // 这边几个channel的数据一起发送，是不是有点浪费了？
  sourceA.io.req := schedule.a
  sourceB.io.req := schedule.b
  sourceC.io.req := schedule.c
  sourceD.io.req := schedule.d
  sourceE.io.req := schedule.e
  sourceX.io.req := schedule.x
  directory.io.write := schedule.dir

  // nested transaction completion是啥？
  // Forward meta-data changes from nested transaction completion
  // 如果是C或者BC的schedule，那么其他基本模块儿的meta data可能得跟着改改
  // 选中的是不是最后一个通道的C mshr
  val select_c  = mshr_selectOH(params.mshrs-1)
  // 选中的是不是倒数第二个通道的BC mshr
  val select_bc = mshr_selectOH(params.mshrs-2)
  nestedwb.set   := Mux(select_c, c_mshr.io.status.bits.set, bc_mshr.io.status.bits.set)
  nestedwb.tag   := Mux(select_c, c_mshr.io.status.bits.tag, bc_mshr.io.status.bits.tag)
  // 这边是要将state改成N或者B
  nestedwb.b_toN       := select_bc && bc_mshr.io.schedule.bits.dir.valid && bc_mshr.io.schedule.bits.dir.bits.data.state === MetaData.INVALID
  nestedwb.b_toB       := select_bc && bc_mshr.io.schedule.bits.dir.valid && bc_mshr.io.schedule.bits.dir.bits.data.state === MetaData.BRANCH
  // 这个是clear dirty
  nestedwb.b_clr_dirty := select_bc && bc_mshr.io.schedule.bits.dir.valid
  // set dirty
  // 这些都是啥？
  nestedwb.c_set_dirty := select_c  &&  c_mshr.io.schedule.bits.dir.valid && c_mshr.io.schedule.bits.dir.bits.data.dirty

  // Pick highest priority request
  // 这里才是真正的将request分发到mshr
  val request = Wire(Decoupled(new FullRequest(params)))
  // request只有在directory valid时才有效？
  // 所以有可能是进mshr时，把directory给读了？
  // 需要单独分配mshr的请求有inner A，X，还有C
  // 我感觉对外如果支持一致性的话，或许得把sinkB也搞到这里来？
  request.valid := directory.io.ready && (sinkA.io.req.valid || sinkX.io.req.valid || sinkB.io.req.valid || sinkC.io.req.valid)
  request.bits := Mux(sinkC.io.req.valid, sinkC.io.req.bits,
                  Mux(sinkB.io.req.valid, sinkB.io.req.bits,
                  Mux(sinkX.io.req.valid, sinkX.io.req.bits, sinkA.io.req.bits)))
  // 这边的几个sink可能会同时valid，然后这里的优先级是C最高，X次之，A最低，这是为啥呢？
  sinkC.io.req.ready := directory.io.ready && request.ready
  sinkB.io.req.ready := directory.io.ready && request.ready && !sinkC.io.req.valid
  sinkX.io.req.ready := directory.io.ready && request.ready && !sinkC.io.req.valid && !sinkB.io.req.valid
  sinkA.io.req.ready := directory.io.ready && request.ready && !sinkC.io.req.valid && !sinkB.io.req.valid && !sinkX.io.req.valid

  // If no MSHR has been assigned to this set, we need to allocate one
  val setMatches = Cat(mshrs.map { m => m.io.status.valid && m.io.status.bits.set === request.bits.set }.reverse)
  // 没有set match的
  val alloc = !setMatches.orR() // NOTE: no matches also means no BC or C pre-emption on this set
  // If a same-set MSHR says that requests of this type must be blocked (for bounded time), do it
  // prio是啥呢？
  // 如果某一个和request set match，那就把blockB、C选出来
  // prio 0 1 2都是什么鬼东西？
  // 主要是这里的prio是啥东西啊？
  // 现在prio 2的是C
  // 还没有prio是1的，1估计就是B
  // 估计就是各个请求根据prio的bit，来标识是acquire，probe还是release
  // 毕竟主体流程其实就三种
  // 每个人可以喝说自己这个mshr是否要block
  // 估计prio就是priority？
  // X和A显然是最低的，B次之，C最高？
  val blockB = Mux1H(setMatches, mshrs.map(_.io.status.bits.blockB)) && request.bits.prio(1)
  val blockC = Mux1H(setMatches, mshrs.map(_.io.status.bits.blockC)) && request.bits.prio(2)

  // If a same-set MSHR says that requests of this type must be handled out-of-band, use special BC|C MSHR
  // ... these special MSHRs interlock the MSHR that said it should be pre-empted.
  // 估计是每个mshr可以说自己这个是否要nest？
  // 估计假如不支持nest，就要另外分配mshr了？
  val nestB  = Mux1H(setMatches, mshrs.map(_.io.status.bits.nestB))  && request.bits.prio(1)
  val nestC  = Mux1H(setMatches, mshrs.map(_.io.status.bits.nestC))  && request.bits.prio(2)
  // Prevent priority inversion; we may not queue to MSHRs beyond our level
  // priority inversion到底是个什么问题？
  // 这尼玛写错了吧？中间的为啥是零，不是应该是bc吗？
  // 低位的全部是1
  val prioFilter = Cat(request.bits.prio(2), !request.bits.prio(0), ~UInt(0, width = params.mshrs-2))
  val lowerMatches = setMatches & prioFilter
  // 如果这个request暂时没法处理，那就放进buffer
  // 这边是确定要不要queue？
  // 假如不是queue，那去哪儿呢？
  // If we match an MSHR <= our priority that neither blocks nor nests us, queue to it.
  // MSHR的priority是啥？估计就是channel的priority？
  // 仔细分析一下tilelink的请求流程主要就三个：
  // 1. acquire
  // 2. probe
  // 3. release
  // 其他几个channel都是之前channel的response，是从属于之前的channel的，不需要额外分配资源，不需要阻塞。
  // 只有A B C这几个请求开启流程是需要额外分配资源的，所以也就因此有优先级要遵守？
  // A请求不能阻塞B，是显然的，典型的例子就是
  // 可能blockB，nestC的意思是，它们现在支持B或者C进来？
  val queue = lowerMatches.orR() && !nestB && !nestC && !blockB && !blockC

  if (!params.lastLevel) {
    params.ccover(request.valid && blockB, "SCHEDULER_BLOCKB", "Interlock B request while resolving set conflict")
    params.ccover(request.valid && nestB,  "SCHEDULER_NESTB", "Priority escalation from channel B")
  }
  if (!params.firstLevel) {
    params.ccover(request.valid && blockC, "SCHEDULER_BLOCKC", "Interlock C request while resolving set conflict")
    params.ccover(request.valid && nestC,  "SCHEDULER_NESTC", "Priority escalation from channel C")
  }
  params.ccover(request.valid && queue, "SCHEDULER_SECONDARY", "Enqueue secondary miss")

  // 下面这一拨儿应该就是进队以及出队的管理

  // It might happen that lowerMatches has >1 bit if the two special MSHRs are in-use
  // We want to Q to the highest matching priority MSHR.
  val lowerMatches1 =
    Mux(lowerMatches(params.mshrs-1), UInt(1 << (params.mshrs-1)),
    Mux(lowerMatches(params.mshrs-2), UInt(1 << (params.mshrs-2)),
    lowerMatches))

  // 如果这个新请求是进之前被schedule的mshr？
  // If this goes to the scheduled MSHR, it may need to be bypassed
  // Alternatively, the MSHR may be refilled from a request queued in the ListBuffer
  // 要和可能选择是从之前的队列里面给pop一项出来
  val selected_requests = Cat(mshr_selectOH, mshr_selectOH, mshr_selectOH) & requests.io.valid
  val a_pop = selected_requests((0 + 1) * params.mshrs - 1, 0 * params.mshrs).orR()
  val b_pop = selected_requests((1 + 1) * params.mshrs - 1, 1 * params.mshrs).orR()
  val c_pop = selected_requests((2 + 1) * params.mshrs - 1, 2 * params.mshrs).orR()
  val bypassMatches = (mshr_selectOH & lowerMatches1).orR() &&
                      Mux(c_pop || request.bits.prio(2), !c_pop, Mux(b_pop || request.bits.prio(1), !b_pop, !a_pop))
  val may_pop = a_pop || b_pop || c_pop
  val bypass = request.valid && queue && bypassMatches
  val will_reload = schedule.reload && (may_pop || bypass)
  val will_pop = schedule.reload && may_pop && !bypass

  params.ccover(mshr_selectOH.orR && bypass, "SCHEDULER_BYPASS", "Bypass new request directly to conflicting MSHR")
  params.ccover(mshr_selectOH.orR && will_reload, "SCHEDULER_RELOAD", "Back-to-back service of two requests")
  params.ccover(mshr_selectOH.orR && will_pop, "SCHEDULER_POP", "Service of a secondary miss")

  // 这里其实就是，请求要么是从外面来的，要么是从队列里面选出来的
  // Repeat the above logic, but without the fan-in
  mshrs.zipWithIndex.foreach { case (m, i) =>
    val sel = mshr_selectOH(i)
    m.io.schedule.ready := sel
    val a_pop = requests.io.valid(params.mshrs * 0 + i)
    val b_pop = requests.io.valid(params.mshrs * 1 + i)
    val c_pop = requests.io.valid(params.mshrs * 2 + i)
    val bypassMatches = lowerMatches1(i) &&
                        Mux(c_pop || request.bits.prio(2), !c_pop, Mux(b_pop || request.bits.prio(1), !b_pop, !a_pop))
    val may_pop = a_pop || b_pop || c_pop
    val bypass = request.valid && queue && bypassMatches
    val will_reload = m.io.schedule.bits.reload && (may_pop || bypass)
    m.io.allocate.bits := Mux(bypass, Wire(new QueuedRequest(params), init = request.bits), requests.io.data)
    m.io.allocate.bits.set := m.io.status.bits.set
    m.io.allocate.bits.repeat := m.io.allocate.bits.tag === m.io.status.bits.tag
    m.io.allocate.valid := sel && will_reload
  }

  // Determine which of the queued requests to pop (supposing will_pop)
  val prio_requests = ~(~requests.io.valid | (requests.io.valid >> params.mshrs) | (requests.io.valid >> 2*params.mshrs))
  val pop_index = OHToUInt(Cat(mshr_selectOH, mshr_selectOH, mshr_selectOH) & prio_requests)
  requests.io.pop.valid := will_pop
  requests.io.pop.bits  := pop_index

  // Reload from the Directory if the next MSHR operation changes tags
  val lb_tag_mismatch = scheduleTag =/= requests.io.data.tag
  val mshr_uses_directory_assuming_no_bypass = schedule.reload && may_pop && lb_tag_mismatch
  val mshr_uses_directory_for_lb = will_pop && lb_tag_mismatch
  val mshr_uses_directory = will_reload && scheduleTag =/= Mux(bypass, request.bits.tag, requests.io.data.tag)

  // Is there an MSHR free for this request?
  val mshr_validOH = Cat(mshrs.map(_.io.status.valid).reverse)
  val mshr_free = (~mshr_validOH & prioFilter).orR()

  // Fanout the request to the appropriate handler (if any)
  val bypassQueue = schedule.reload && bypassMatches
  val request_alloc_cases =
     (alloc && !mshr_uses_directory_assuming_no_bypass && mshr_free) ||
     (nestB && !mshr_uses_directory_assuming_no_bypass && !bc_mshr.io.status.valid && !c_mshr.io.status.valid) ||
     (nestC && !mshr_uses_directory_assuming_no_bypass && !c_mshr.io.status.valid)
  request.ready := request_alloc_cases || (queue && (bypassQueue || requests.io.push.ready))
  val alloc_uses_directory = request.valid && request_alloc_cases

  // When a request goes through, it will need to hit the Directory
  directory.io.read.valid := mshr_uses_directory || alloc_uses_directory
  directory.io.read.bits.set := Mux(mshr_uses_directory_for_lb, scheduleSet,          request.bits.set)
  directory.io.read.bits.tag := Mux(mshr_uses_directory_for_lb, requests.io.data.tag, request.bits.tag)

  // Enqueue the request if not bypassed directly into an MSHR
  requests.io.push.valid := request.valid && queue && !bypassQueue
  requests.io.push.bits.data  := request.bits
  requests.io.push.bits.index := Mux1H(
    request.bits.prio, Seq(
      OHToUInt(lowerMatches1 << params.mshrs*0),
      OHToUInt(lowerMatches1 << params.mshrs*1),
      OHToUInt(lowerMatches1 << params.mshrs*2)))

  val mshr_insertOH = ~(leftOR(~mshr_validOH) << 1) & ~mshr_validOH & prioFilter
  (mshr_insertOH.asBools zip mshrs) map { case (s, m) =>
    when (request.valid && alloc && s && !mshr_uses_directory_assuming_no_bypass) {
      m.io.allocate.valid := Bool(true)
      m.io.allocate.bits := request.bits
      m.io.allocate.bits.repeat := Bool(false)
    }
  }

  when (request.valid && nestB && !bc_mshr.io.status.valid && !c_mshr.io.status.valid && !mshr_uses_directory_assuming_no_bypass) {
    bc_mshr.io.allocate.valid := Bool(true)
    bc_mshr.io.allocate.bits := request.bits
    bc_mshr.io.allocate.bits.repeat := Bool(false)
    assert (!request.bits.prio(0))
  }
  bc_mshr.io.allocate.bits.prio(0) := Bool(false)

  when (request.valid && nestC && !c_mshr.io.status.valid && !mshr_uses_directory_assuming_no_bypass) {
    c_mshr.io.allocate.valid := Bool(true)
    c_mshr.io.allocate.bits := request.bits
    c_mshr.io.allocate.bits.repeat := Bool(false)
    assert (!request.bits.prio(0))
    assert (!request.bits.prio(1))
  }
  c_mshr.io.allocate.bits.prio(0) := Bool(false)
  c_mshr.io.allocate.bits.prio(1) := Bool(false)

  // Fanout the result of the Directory lookup
  val dirTarget = Mux(alloc, mshr_insertOH, Mux(nestB, UInt(1 << (params.mshrs-2)), UInt(1 << (params.mshrs-1))))
  val directoryFanout = params.dirReg(RegNext(Mux(mshr_uses_directory, mshr_selectOH, Mux(alloc_uses_directory, dirTarget, UInt(0)))))
  mshrs.zipWithIndex.foreach { case (m, i) =>
    m.io.directory.valid := directoryFanout(i)
    m.io.directory.bits := directory.io.result.bits
  }

  // MSHR response meta-data fetch
  sinkC.io.way :=
    Mux(bc_mshr.io.status.valid && bc_mshr.io.status.bits.set === sinkC.io.set,
      bc_mshr.io.status.bits.way,
      Mux1H(abc_mshrs.map(m => m.io.status.valid && m.io.status.bits.set === sinkC.io.set),
            abc_mshrs.map(_.io.status.bits.way)))
  sinkD.io.way := Vec(mshrs.map(_.io.status.bits.way))(sinkD.io.source)
  sinkD.io.set := Vec(mshrs.map(_.io.status.bits.set))(sinkD.io.source)

  // Beat buffer connections between components
  sinkA.io.pb_pop <> sourceD.io.pb_pop
  sourceD.io.pb_beat := sinkA.io.pb_beat
  sinkC.io.rel_pop <> sourceD.io.rel_pop
  sourceD.io.rel_beat := sinkC.io.rel_beat

  // BankedStore ports
  bankedStore.io.sinkC_adr <> sinkC.io.bs_adr
  bankedStore.io.sinkC_dat := sinkC.io.bs_dat
  bankedStore.io.sinkD_adr <> sinkD.io.bs_adr
  bankedStore.io.sinkD_dat := sinkD.io.bs_dat
  bankedStore.io.sourceC_adr <> sourceC.io.bs_adr
  bankedStore.io.sourceD_radr <> sourceD.io.bs_radr
  bankedStore.io.sourceD_wadr <> sourceD.io.bs_wadr
  bankedStore.io.sourceD_wdat := sourceD.io.bs_wdat
  sourceC.io.bs_dat := bankedStore.io.sourceC_dat
  sourceD.io.bs_rdat := bankedStore.io.sourceD_rdat

  // SourceD data hazard interlock
  sourceD.io.evict_req := sourceC.io.evict_req
  sourceD.io.grant_req := sinkD  .io.grant_req
  sourceC.io.evict_safe := sourceD.io.evict_safe
  sinkD  .io.grant_safe := sourceD.io.grant_safe

  private def afmt(x: AddressSet) = s"""{"base":${x.base},"mask":${x.mask}}"""
  private def addresses = params.inner.manager.managers.flatMap(_.address).map(afmt _).mkString(",")
  private def setBits = params.addressMapping.drop(params.offsetBits).take(params.setBits).mkString(",")
  private def tagBits = params.addressMapping.drop(params.offsetBits + params.setBits).take(params.tagBits).mkString(",")
  private def simple = s""""reset":"${reset.pathName}","tagBits":[${tagBits}],"setBits":[${setBits}],"blockBytes":${params.cache.blockBytes},"ways":${params.cache.ways}"""
  def json: String = s"""{"addresses":[${addresses}],${simple},"directory":${directory.json},"subbanks":${bankedStore.json}}"""
}
