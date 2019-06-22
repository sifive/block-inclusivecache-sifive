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

package freechips.rocketchip.diplomaticobjectmodel.logicaltree

import diplomaticobjectmodel.model.{OMCacheMaster, OMInclusiveCache}
import freechips.rocketchip.diplomacy.{Edges, ResourceBindings, ResourceBindingsMap, SimpleDevice}
import freechips.rocketchip.diplomaticobjectmodel._
import freechips.rocketchip.diplomaticobjectmodel.model._
import sifive.blocks.inclusivecache.{InclusiveCache, CacheParameters, InclusiveCacheMicroParameters}
import freechips.rocketchip.tilelink.{TLAdapterNode, TLEdgeIn, TLEdgeOut}


class InclusiveCacheLogicalTreeNode(
  device: SimpleDevice,
  cache: CacheParameters,
  micro: InclusiveCacheMicroParameters,
  nBanks: Int,
  node: TLAdapterNode,
  regMap: => Option[OMRegisterMap]
) extends LogicalTreeNode(() => Some(device)) {

  def getOMInclusiveCache(resourceBindings: ResourceBindings): Seq[OMComponent] = {
    val memRegions= DiplomaticObjectModelAddressing.getOMMemoryRegions("InclusiveCache", resourceBindings, regMap)
    Seq(OMInclusiveCache (
      memoryRegions = memRegions,
      nSets = cache.sets,
      nWays = cache.ways,
      blockSizeBytes = cache.blockBytes,
      dataMemorySizeBytes = cache.sets * cache.ways * cache.blockBytes * nBanks,
      nBanks = nBanks,
      innerBytes = node.edges.in.head.manager.beatBytes,
      outerBytes = node.edges.out.head.manager.beatBytes
    ))
  }

  override def getOMComponents(resourceBindings: ResourceBindings, children: Seq[OMComponent]): Seq[OMComponent] = {
    DiplomaticObjectModelAddressing.getOMComponentHelper(resourceBindings, getOMInclusiveCache)
  }
}
