{-
© 2012 Johan Kiviniemi <devel@johan.kiviniemi.name>

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-}

{-# LANGUAGE Rank2Types #-}

module Generate.SQL (withDB) where

import           Control.Exception
import           Control.Monad
import           Database.HDBC
import           Database.HDBC.Sqlite3

withDB :: (forall conn. IConnection conn => conn -> IO a) -> IO a
withDB act =
  bracket (connectSqlite3 ":memory:") disconnect $ \conn -> do
    initSchema conn
    loadData conn

    act conn

initSchema :: IConnection conn => conn -> IO ()
initSchema conn =
  outsideTransaction conn $ \_ ->
    runRaw conn =<< readFile "sqlite_schema.sql"

loadData :: IConnection conn => conn -> IO ()
loadData conn =
  forM_ ["mpu_pins", "signals", "mpu_pins_signals", "bb_pins"] $ \table ->
    withTransaction conn $ \_ ->
      runRaw conn =<< readFile ("data/" ++ table ++ ".sql")

-- A nice kluge.
outsideTransaction :: IConnection conn => conn -> (conn -> IO a) -> IO a
outsideTransaction conn act =
  do { runRaw conn "commit"; act conn } `finally` runRaw conn "begin"
