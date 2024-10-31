module Deserialise where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Control.Monad.Error.Class (MonadError)
import Data.Aeson (toJSON, (.=))
import Data.Aeson qualified as Json
import Data.Base64.Types qualified as Base64
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Short qualified as BS
import Data.Function ((&))
import Data.IntCast (intCast)
import Data.Some (withSome)
import Data.String.Interpolate (i)
import Data.Vector qualified as Vector
import Database qualified as DB
import Database.PostgreSQL.Simple (Connection)
import Numeric.Natural (Natural)
import PlutusCore (DefaultUni (..), ValueOf (..))
import PlutusCore.Default (noMoreTypeFunctions)
import PlutusCore.Default qualified as U
import PlutusCore.Pretty (pretty, render)
import PlutusLedgerApi.Common (Data (..), ScriptNamedDeBruijn (..))
import PlutusLedgerApi.Common qualified as P
import PlutusPrelude (showText)
import UntypedPlutusCore qualified as U

deserialiseScripts :: Connection -> IO ()
deserialiseScripts conn = do
  n <- deserialiseBatch conn 0
  putStrLn [i|Inserted #{n} de-serialised scripts.|]

deserialiseBatch :: Connection -> Int -> IO Int
deserialiseBatch conn insertedOverall = do
  let batchSize :: Natural = 1000
  -- 1000 is chosen to be somewhere between 1 and the maximum amount of records
  -- which fits in memory.
  -- It would be too much overhead to process one record at a time, and app
  -- would be too slow (possibly crashing) if we processed all records at once.
  serialisedRecords :: [DB.SerialisedScriptRecord] <-
    DB.selectSerialisedScriptsToDeserialise conn batchSize
  deserialisedRecords <-
    traverse deserialiseScript serialisedRecords
      & either throwIO pure
  numInserted <- DB.insertDeserialisedScripts conn deserialisedRecords
  let insertedOverall' = insertedOverall + numInserted
  unless (numInserted == 0) do
    putStrLn
      [i|#{insertedOverall'} scripts deserialised (+#{numInserted}).|]
  if numInserted < fromIntegral batchSize
    then pure insertedOverall'
    else deserialiseBatch conn insertedOverall'

deserialiseScript
  :: (MonadError P.ScriptDecodeError m)
  => DB.SerialisedScriptRecord
  -> m DB.DeserialisedScriptRecord
deserialiseScript
  ( DB.MkSerialisedScriptRecord
      hash
      ledgerLang
      (P.MajorProtocolVersion . intCast -> majorProtocolVer)
      (BS.toShort -> serialised)
    ) = do
    scriptForEval <- P.deserialiseScript ledgerLang majorProtocolVer serialised
    let ScriptNamedDeBruijn uplc = P.deserialisedScript scriptForEval
    pure . DB.MkDeserialisedScriptRecord hash . termToJson $ U._progTerm uplc

termToJson :: U.Term U.NamedDeBruijn U.DefaultUni U.DefaultFun () -> Json.Value
termToJson = \case
  U.Var () name ->
    Json.object
      [ "ctor" .= Json.String "Var"
      , "name" .= U.ndbnString name
      ]
  U.LamAbs () name body ->
    Json.object
      [ "ctor" .= Json.String "LamAbs"
      , "name" .= U.ndbnString name
      , "body" .= termToJson body
      ]
  U.Apply () fun arg ->
    Json.object
      [ "ctor" .= Json.String "Apply"
      , "fun" .= termToJson fun
      , "arg" .= termToJson arg
      ]
  U.Force () term ->
    Json.object
      [ "ctor" .= Json.String "Force"
      , "term" .= termToJson term
      ]
  U.Delay () term ->
    Json.object
      [ "ctor" .= Json.String "Delay"
      , "term" .= termToJson term
      ]
  U.Constant () defaultUniValue ->
    Json.object
      [ "ctor" .= Json.String "Constant"
      , "tag" .= withSome defaultUniValue valueOfToJsonTag
      , "val" .= withSome defaultUniValue valueOfToJson
      ]
  U.Builtin () fun ->
    Json.object
      [ "ctor" .= Json.String "Builtin"
      , "fun" .= show fun
      ]
  U.Error () ->
    Json.object
      [ "ctor" .= Json.String "Error"
      ]
  U.Constr () tag terms ->
    Json.object
      [ "ctor" .= Json.String "Constr"
      , "tag" .= tag
      , "terms" .= map termToJson terms
      ]
  U.Case () scrutinee branches ->
    Json.object
      [ "ctor" .= Json.String "Case"
      , "scrutinee" .= termToJson scrutinee
      , "branches" .= fmap termToJson branches
      ]

valueOfToJsonTag :: U.ValueOf U.DefaultUni a -> Json.Value
valueOfToJsonTag (ValueOf uni _value) = Json.String (render (pretty uni))

valueOfToJson :: U.ValueOf U.DefaultUni a -> Json.Value
valueOfToJson = \case
  ValueOf DefaultUniInteger int ->
    Json.String (showText int)
  ValueOf DefaultUniByteString bs ->
    toJSON (Base64.extractBase64 (Base64.encodeBase64 bs))
  ValueOf DefaultUniString s ->
    Json.String s
  ValueOf DefaultUniUnit () ->
    Json.String "()"
  ValueOf DefaultUniBool b ->
    Json.Bool b
  ValueOf (DefaultUniApply DefaultUniProtoList uni) xs ->
    Json.Array (Vector.fromList (valueOfToJson . ValueOf uni <$> xs))
  ValueOf
    (DefaultUniApply (DefaultUniApply DefaultUniProtoPair uniA) uniB)
    (a, b) ->
      Json.object
        [ "fst" .= valueOfToJson (ValueOf uniA a)
        , "snd" .= valueOfToJson (ValueOf uniB b)
        ]
  ValueOf (DefaultUniApply (DefaultUniApply (DefaultUniApply f _) _) _) _ ->
    noMoreTypeFunctions f
  ValueOf DefaultUniData d ->
    dataToJson d
  ValueOf DefaultUniBLS12_381_G1_Element e ->
    Json.object
      [ "tag" .= Json.String "bls12_381_g1_element"
      , "value" .= showText e
      ]
  ValueOf DefaultUniBLS12_381_G2_Element e ->
    Json.object
      [ "tag" .= Json.String "bls12_381_g2_element"
      , "value" .= showText e
      ]
  ValueOf DefaultUniBLS12_381_MlResult r ->
    Json.object
      [ "tag" .= Json.String "bls12_381_ml_result"
      , "value" .= showText r
      ]

defaultUniToJson :: DefaultUni a -> Json.Value
defaultUniToJson = \case
  DefaultUniInteger -> Json.String "integer"
  DefaultUniByteString -> Json.String "bytestring"
  DefaultUniString -> Json.String "string"
  DefaultUniUnit -> Json.String "unit"
  DefaultUniBool -> Json.String "bool"
  DefaultUniProtoList -> Json.String "list"
  DefaultUniProtoPair -> "pair"
  DefaultUniApply uniF uniA ->
    Json.object
      [ "ctor" .= Json.String "apply"
      , "fun" .= defaultUniToJson uniF
      , "arg" .= defaultUniToJson uniA
      ]
  DefaultUniData -> Json.String "data"
  DefaultUniBLS12_381_G1_Element -> Json.String "bls12_381_g1_element"
  DefaultUniBLS12_381_G2_Element -> Json.String "bls12_381_g2_element"
  DefaultUniBLS12_381_MlResult -> Json.String "bls12_381_ml_result"

dataToJson :: P.Data -> Json.Value
dataToJson = \case
  Constr int datas ->
    Json.object
      [ "ctor" .= Json.String "Constr"
      , "int" .= int
      , "args" .= map dataToJson datas
      ]
  Map pairs ->
    Json.object
      [ "ctor" .= Json.String "Map"
      , "elems"
          .= map
            ( \(k, v) ->
                Json.object ["k" .= dataToJson k, "v" .= dataToJson v]
            )
            pairs
      ]
  List datas ->
    Json.object
      [ "ctor" .= Json.String "List"
      , "items" .= map dataToJson datas
      ]
  I int ->
    Json.object
      [ "ctor" .= Json.String "I"
      , "int" .= int
      ]
  B bs ->
    Json.object
      [ "ctor" .= Json.String "B"
      , "bytes" .= Base64.extractBase64 (Base64.encodeBase64 bs)
      ]
