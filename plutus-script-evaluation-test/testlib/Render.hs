module Render where

import Cardano.Api.Shelley qualified as C
import Data.ByteString.Base16 qualified as B16
import Data.String.Interpolate (__i)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

blockHash :: C.Hash C.BlockHeader -> String
blockHash =
  Text.unpack . Text.decodeLatin1 . B16.encode . C.serialiseToRawBytes

startChainPoint :: C.ChainPoint -> String
startChainPoint chainPoint =
  let slotNo = C.chainPointToSlotNo chainPoint
      headerHash = C.chainPointToHeaderHash chainPoint
   in [__i|
      Starting from checkpoint:
        Slot: #{maybe "Origin" show slotNo}
        Hash: #{maybe "Origin" blockHash headerHash} |]

chainPointSlot :: C.ChainPoint -> String
chainPointSlot = \case
  C.ChainPointAtGenesis -> "Genesis"
  C.ChainPoint slotNo _header -> show slotNo
