package diplomaticobjectmodel.model

import freechips.rocketchip.diplomacy.ResourceBindings
import freechips.rocketchip.diplomaticobjectmodel.{DiplomaticObjectModelAddressing, DiplomaticObjectModelUtils}
import freechips.rocketchip.diplomaticobjectmodel.model._
import freechips.rocketchip.util.Code

case class OMCacheMaster(
  id: Int,  // The ID of the master
  documentationName: String,
  _types: Seq[String] = Seq("OMCacheMaster", "OMCompoundType")
) extends OMCompoundType
