module Main (main) where

import Deserialise (deserialiseScripts)
import Main.Utf8 (withUtf8)
import Options (parserInfo)
import Options.Applicative (execParser)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdin, stdout)

main :: IO ()
main = withUtf8 do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  deserialiseScripts =<< execParser parserInfo
