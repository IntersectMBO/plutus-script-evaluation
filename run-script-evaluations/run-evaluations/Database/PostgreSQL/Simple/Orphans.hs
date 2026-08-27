{-# OPTIONS_GHC -Wno-orphans #-}

module Database.PostgreSQL.Simple.Orphans where

import Control.Exception (Exception)
import Data.Int (Int64)
import Data.SatInt (unsafeToSatInt)
import Database.PostgreSQL.Simple.FromField (FromField (..), conversionError)
import PlutusLedgerApi.Common (
  ExCPU (..),
  ExMemory (..),
  MajorProtocolVersion (..),
  PlutusLedgerLanguage (..),
  SatInt (..),
 )

instance FromField PlutusLedgerLanguage where
  -- The indexer writes this column from cardano-ledger's 'Language'
  -- (constructor index + 1), so both enumerations must keep the same
  -- constructor order.
  fromField f mdata = do
    i :: Int <- fromField f mdata
    case i of
      1 -> pure PlutusV1
      2 -> pure PlutusV2
      3 -> pure PlutusV3
      4 -> pure PlutusV4
      n -> conversionError (InvalidPlutusLedgerLanguage n)

newtype InvalidPlutusLedgerLanguage = InvalidPlutusLedgerLanguage Int
  deriving stock (Show)
  deriving anyclass (Exception)

deriving newtype instance FromField MajorProtocolVersion
deriving newtype instance FromField ExMemory
deriving newtype instance FromField ExCPU

instance FromField SatInt where
  fromField f mdata = unsafeToSatInt <$> fromField @Int64 f mdata
