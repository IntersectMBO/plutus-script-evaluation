module Materialise (materialiseViews) where

import Control.Monad (void)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Connection, execute_)

materialiseViews :: Connection -> IO ()
materialiseViews conn = do
  refreshMaterializedView "builtin_version_num_scripts"
  refreshMaterializedView "builtin_version_num_usages"
 where
  refreshMaterializedView :: String -> IO ()
  refreshMaterializedView viewName = do
    putStrLn $ "Refreshing materialized view: " <> viewName
    void $ execute_ conn $ fromString $ "REFRESH MATERIALIZED VIEW " <> viewName
