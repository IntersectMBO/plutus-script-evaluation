{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Types (
  Checkpoint (..),
  Block,
  OnImmutableTip (..),
)
where

import Cardano.Api qualified as Cardano
import Codec.Serialise qualified as CBOR
import Codec.Serialise.Decoding qualified as CBOR
import Codec.Serialise.Encoding qualified as CBOR
import Data.Proxy (Proxy (Proxy))

-- | A checkpoint from which the streamer can resume.
data Checkpoint = Checkpoint
  { cChainPoint :: Cardano.ChainPoint
  , cLedgerState :: Cardano.LedgerState
  }

instance CBOR.Serialise Checkpoint where
  encode (Checkpoint chainPoint ledgerState) =
    mconcat
      [ CBOR.encodeListLen 2
      , encodeChainPoint chainPoint
      , Cardano.encodeLedgerState  ledgerState
      ]
  decode = do
    CBOR.decodeListLenOf 2
    Checkpoint <$> decodeChainPoint <*> Cardano.decodeLedgerState 

encodeChainPoint :: Cardano.ChainPoint -> CBOR.Encoding
encodeChainPoint p = CBOR.encode $ case p of
  Cardano.ChainPointAtGenesis -> Nothing
  Cardano.ChainPoint slot hash -> Just (slot, Cardano.serialiseToRawBytes hash)

decodeChainPoint :: CBOR.Decoder s Cardano.ChainPoint
decodeChainPoint =
  CBOR.decode >>= \case
    Nothing -> pure Cardano.ChainPointAtGenesis
    Just (slot, hashRawBytes) ->
      either
        (\err -> fail ("decodeChainPoint is unable to decode block hash: " <> show err))
        (pure . Cardano.ChainPoint slot)
        ( Cardano.deserialiseFromRawBytes
            (Cardano.proxyToAsType (Proxy @(Cardano.Hash Cardano.BlockHeader)))
            hashRawBytes
        )

type Block = Cardano.BlockInMode

data OnImmutableTip = OnImmutableTipExit | OnImmutableTipPause
  deriving (Eq, Show)
