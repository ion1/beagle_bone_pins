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

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}

module Generate.SQL (withPinInfo, withDB, genPinInfo) where

import           Control.Exception
import           Control.Monad
import           Data.Convertible      (Convertible)
import qualified Data.Map              as Map
import           Database.HDBC
import           Database.HDBC.Sqlite3

import           Generate.Types

withPinInfo :: (PinInfo -> IO a) -> IO a
withPinInfo act = withDB (genPinInfo >=> act)

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

genPinInfo :: IConnection conn => conn -> IO PinInfo
genPinInfo conn = do
  bbPins  <- genBBPins  conn
  mpuPins <- genMPUPins conn
  signals <- genSignals conn

  return $ PinInfo { piBBPins  = bbPins
                   , piMPUPins = mpuPins
                   , piSignals = signals
                   }

genBBPins :: IConnection conn => conn -> IO (Map.Map BBPinId BBPin)
genBBPins conn = do
  sth  <- prepare conn "select id, name, mpu_pin_id from bb_pins"
  _    <- execute sth []
  rows <- fetchAllRowsMap sth

  fmap Map.fromList . forM rows $ \row ->
    return ( val row "id"
           , BBPin { bbpId       = val row "id"
                   , bbpName     = val row "name"
                   , bbpMPUPinId = val row "mpu_pin_id"
                   }
           )

genMPUPins :: IConnection conn => conn -> IO (Map.Map MPUPinId MPUPin)
genMPUPins conn = do
  sth  <- prepare conn "select id, linux_name from mpu_pins"
  _    <- execute sth []
  rows <- fetchAllRowsMap sth

  sthS <- prepare conn
            "select mode, signal_id from mpu_pins_signals where mpu_pin_id = ?"

  fmap Map.fromList . forM rows $ \row -> do
    _     <- execute sthS [val row "id"]
    rowsS <- fetchAllRowsMap sthS

    signals <- fmap Map.fromList . forM rowsS $ \rowS ->
      return ( val rowS "signal_id"
             , MPUPinSignal { mpsMode     = val rowS "mode"
                            , mpsSignalId = val rowS "signal_id"
                            }
             )

    return ( val row "id"
           , MPUPin { mpId = val row "id"
                    , mpLinuxName = val row "linux_name"
                    , mpSignals = signals
                    }
           )

genSignals :: IConnection conn => conn -> IO (Map.Map SignalId Signal)
genSignals conn = do
  sth  <- prepare conn
            "select id, signal_type_id, gpio_num, linux_pwm_name from signals"
  _    <- execute sth []
  rows <- fetchAllRowsMap sth

  fmap Map.fromList . forM rows $ \row ->
    return ( val row "id"
           , Signal { sId           = val row "id"
                    , sType         = val row "signal_type_id"
                    , sGPIONum      = val row "gpio_num"
                    , sLinuxPWMName = val row "linux_pwm_name"
                    }
           )

val :: Convertible SqlValue a => Map.Map String SqlValue -> String -> a
val row name = fromSql (row Map.! name)

-- A nice kluge.
outsideTransaction :: IConnection conn => conn -> (conn -> IO a) -> IO a
outsideTransaction conn act =
  do { runRaw conn "commit"; act conn } `finally` runRaw conn "begin"
