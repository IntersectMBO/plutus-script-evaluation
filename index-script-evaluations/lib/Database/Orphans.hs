{-# OPTIONS_GHC -Wno-orphans #-}

module Database.Orphans () where

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (SlotNo (..), unSlotNo)
import Data.Bits (Bits (shiftR, xor, (.&.)))
import Data.Coerce (coerce)
import Data.Digest.Murmur64 (Hash64, asWord64)
import Data.Int (Int64)
import Data.IntCast (intCast, intCastIso)
import Data.Maybe (maybeToList)
import Data.Profunctor.Product.Default (Default (..))
import Data.SatInt (fromSatInt, unsafeToSatInt)
import Data.Word (Word64)
import Database.PostgreSQL.Simple.Types ()
import Opaleye (
  DefaultFromField (..),
  Field,
  FieldNullable,
  FromFields,
  SqlInt2,
  SqlInt4,
  SqlInt8,
  ToFields,
  fromPGSFromField,
  toToFields,
  unsafeFromField,
 )
import Opaleye.Internal.HaskellDB.PrimQuery (Literal (IntegerLit))
import Opaleye.Internal.PGTypes (literalColumn)
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (..), ExMemory (..))
import PlutusLedgerApi.Common (
  MajorProtocolVersion (MajorProtocolVersion, getMajorProtocolVersion),
  PlutusLedgerLanguage (..),
  SatInt,
 )
import Unsafe.Coerce (unsafeCoerce)

instance Default ToFields PlutusLedgerLanguage (Field SqlInt2) where
  -- DB counts constructors from 1 while derived Enum instance counts from 0,
  -- so we need to increment by 1 when writing to DB
  def = toToFields (sqlInt2 . succ . fromIntegral . fromEnum)
   where
    sqlInt2 = literalColumn . IntegerLit

instance DefaultFromField SqlInt2 PlutusLedgerLanguage where
  -- DB counts constructors from 1 while derived Enum instance counts from 0,
  -- so we need to decrement by 1 when reading from DB
  defaultFromField = toEnum . pred <$> fromPGSFromField

instance Default ToFields SlotNo (Field SqlInt8) where
  def = toToFields (literalColumn . IntegerLit . fromIntegral . unSlotNo)

instance DefaultFromField SqlInt8 SlotNo where
  defaultFromField =
    -- Casting Int64 to the Word64 is generally unsafe
    -- but in this case we trust that negative slot values aren't in the DB
    unsafeFromField (SlotNo . intCastIso) (defaultFromField @SqlInt8 @Int64)

instance Default ToFields BlockNo (Field SqlInt8) where
  def = toToFields (literalColumn . IntegerLit . fromIntegral . unBlockNo)

instance Default ToFields Hash64 (Field SqlInt8) where
  def = toToFields (literalColumn . IntegerLit . word64ToInteger . asWord64)
   where
    word64ToInteger :: Word64 -> Integer
    word64ToInteger n =
      fromIntegral (shiftR n 1) `xor` negate (fromIntegral (n .&. 1))

instance DefaultFromField SqlInt8 BlockNo where
  defaultFromField =
    -- Casting Int64 to the Word64 is generally unsafe
    -- but in this case we trust that negative block values aren't in the DB
    unsafeFromField (BlockNo . intCastIso) (defaultFromField @SqlInt8 @Int64)

instance DefaultFromField SqlInt8 Hash64 where
  defaultFromField =
    -- Casting Int64 to the Word64 is generally unsafe
    -- but in this case we trust that negative hash values aren't in the DB
    unsafeFromField
      (unsafeCoerce @Word64 . intCastIso) -- Hash64 is a newtype over Word64
      (defaultFromField @SqlInt8 @Int64)

instance DefaultFromField SqlInt4 ExCPU where
  defaultFromField = ExCPU <$> defaultFromField

instance Default ToFields ExCPU (Field SqlInt8) where
  def = toToFields (literalColumn . IntegerLit . fromSatInt . coerce)

instance DefaultFromField SqlInt4 ExMemory where
  defaultFromField = ExMemory <$> defaultFromField

instance Default ToFields ExMemory (Field SqlInt8) where
  def = toToFields (literalColumn . IntegerLit . fromSatInt . coerce)

instance DefaultFromField SqlInt4 SatInt where
  defaultFromField =
    unsafeFromField
      (unsafeToSatInt . intCastIso)
      (defaultFromField @SqlInt4 @Int)

instance Default FromFields (FieldNullable SqlInt8) [Int64] where
  def = maybeToList <$> def

instance Default ToFields MajorProtocolVersion (Field SqlInt2) where
  def =
    toToFields $ literalColumn . IntegerLit . intCast . getMajorProtocolVersion

instance DefaultFromField SqlInt2 MajorProtocolVersion where
  defaultFromField = MajorProtocolVersion <$> fromPGSFromField
