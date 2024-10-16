{-# OPTIONS_GHC -Wno-orphans #-}

module Database.Orphans () where

import Cardano.Slotting.Block (BlockNo (unBlockNo))
import Cardano.Slotting.Slot (SlotNo, unSlotNo)
import Data.Bits (Bits (shiftR, xor, (.&.)))
import Data.Digest.Murmur64 (Hash64, asWord64)
import Data.Int (Int16)
import Data.Profunctor.Product.Default (Default (..))
import Data.Word (Word64)
import Database.PostgreSQL.Simple.Types ()
import Opaleye (Field, SqlInt2, SqlInt8, ToFields, toToFields)
import Opaleye.Internal.HaskellDB.PrimQuery (Literal (IntegerLit))
import Opaleye.Internal.PGTypes (literalColumn)
import PlutusLedgerApi.Common (PlutusLedgerLanguage (..))

instance Default ToFields Int16 (Field SqlInt2) where
  def = toToFields (literalColumn . IntegerLit . fromIntegral)

instance Default ToFields PlutusLedgerLanguage (Field SqlInt2) where
  def = toToFields \case
    PlutusV1 -> sqlInt2 1
    PlutusV2 -> sqlInt2 2
    PlutusV3 -> sqlInt2 3
   where
    sqlInt2 = literalColumn . IntegerLit

instance Default ToFields SlotNo (Field SqlInt8) where
  def = toToFields (literalColumn . IntegerLit . fromIntegral . unSlotNo)

instance Default ToFields BlockNo (Field SqlInt8) where
  def = toToFields (literalColumn . IntegerLit . fromIntegral . unBlockNo)

instance Default ToFields Hash64 (Field SqlInt8) where
  def = toToFields (literalColumn . IntegerLit . word64ToInteger . asWord64)
   where
    word64ToInteger :: Word64 -> Integer
    word64ToInteger n =
      fromIntegral (shiftR n 1) `xor` negate (fromIntegral (n .&. 1))
