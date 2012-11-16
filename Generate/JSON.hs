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
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Generate.JSON (generate) where

import           Control.Applicative
import           Control.Monad              (forM)
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty   as P
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Char                  as Char
import           Data.Convertible           (Convertible)
import           Data.Function              (on)
import qualified Data.Map                   as Map
import           Data.Maybe                 (catMaybes)
import qualified Data.Text                  as Text
import           Database.HDBC

data PinInfo = PinInfo { piBBPins  :: Map.Map String BBPin
                       , piMPUPins :: Map.Map String MPUPin
                       , piSignals :: Map.Map String Signal
                       }
  deriving (Eq, Ord, Show, Read)

data BBPin = BBPin { bbpId       :: String
                   , bbpName     :: String
                   , bbpMPUPinId :: Maybe String
                   }
  deriving (Eq, Ord, Show, Read)

data MPUPin = MPUPin { mpId        :: String
                     , mpLinuxName :: Maybe String
                     , mpSignals   :: Map.Map String MPUPinSignal
                     }
  deriving (Eq, Ord, Show, Read)

data MPUPinSignal = MPUPinSignal { mpsMode     :: Maybe Integer
                                 , mpsSignalId :: String
                                 }
  deriving (Eq, Ord, Show, Read)

data Signal = Signal { sId           :: String
                     , sType         :: String
                     , sGPIONum      :: Maybe Integer
                     , sLinuxPWMName :: Maybe String
                     }
  deriving (Eq, Ord, Show, Read)

instance ToJSON PinInfo where
  toJSON (PinInfo {..}) =
    object [ "bb_pins"  .= piBBPins
           , "mpu_pins" .= piMPUPins
           , "signals"  .= piSignals
           ]

instance ToJSON BBPin where
  toJSON (BBPin {..}) =
    (object . catMaybes) [ ("name"    .=) <$> Just bbpName
                         , ("mpu_pin" .=) <$> bbpMPUPinId
                         ]

instance ToJSON MPUPin where
  toJSON (MPUPin {..}) =
    (object . catMaybes) [ ("linux_name" .=) <$> mpLinuxName
                         , ("signals"    .=) <$> Just mpSignals
                         ]

instance ToJSON MPUPinSignal where
  toJSON (MPUPinSignal {..}) =
    (object . catMaybes) [ ("mode" .=) <$> mpsMode ]

instance ToJSON Signal where
  toJSON (Signal {..}) =
    (object . catMaybes) [ ("type"           .=) <$> Just sType
                         , ("gpio_num"       .=) <$> sGPIONum
                         , ("linux_pwm_name" .=) <$> sLinuxPWMName
                         ]

generate :: IConnection conn => conn -> FilePath -> IO ()
generate conn path = LBS8.writeFile path . P.encodePretty' conf =<< genJSON conn
  where
    conf = P.Config { P.confIndent  = 2
                    , P.confCompare = Just numCompare
                    }

genJSON :: IConnection conn => conn -> IO Value
genJSON conn = do
  bbPins  <- genBBPins  conn
  mpuPins <- genMPUPins conn
  signals <- genSignals conn

  let res = PinInfo { piBBPins  = bbPins
                    , piMPUPins = mpuPins
                    , piSignals = signals
                    }

  return (toJSON res)

genBBPins :: IConnection conn => conn -> IO (Map.Map String BBPin)
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

genMPUPins :: IConnection conn => conn -> IO (Map.Map String MPUPin)
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

genSignals :: IConnection conn => conn -> IO (Map.Map String Signal)
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

data NumSort a = NSNumber Integer
               | NSOther a
  deriving (Eq, Ord, Show, Read)

numCompare :: Text.Text -> Text.Text -> Ordering
numCompare = compare `on` textToNumSort
  where
    textToNumSort = map partToNumSort . Text.groupBy ((==) `on` Char.isDigit)

    partToNumSort t =
      case Text.uncons t of
        Just (c, _) | Char.isDigit c -> NSNumber (read (Text.unpack t))
        _                            -> NSOther t
