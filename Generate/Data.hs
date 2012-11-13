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

{-# LANGUAGE OverloadedStrings #-}

module Generate.Data
( PinData (..)
, BBPinId (..)
, MPUPinId (..)
, SignalId (..)
, BBPin (..)
, MPUPin (..)
, MPUPinSignal (..)
, Signal (..)
, SignalType (..)
, parseData
) where

import           Control.Applicative
import qualified Data.ByteString.Lazy as BSL
import           Data.Csv
import qualified Data.Vector          as Vec
import           System.FilePath      ((</>))

data PinData = PinData { pdBBPins         :: [BBPin]
                       , pdMPUPins        :: [MPUPin]
                       , pdMPUPinsSignals :: [MPUPinSignal]
                       , pdSignals        :: [Signal]
                       }
  deriving (Eq, Ord, Show, Read)

newtype BBPinId  = BBPinId { fromBBPinId :: String }
  deriving (Eq, Ord, Show, Read)
newtype MPUPinId = MPUPinId { fromMPUPinId :: String }
  deriving (Eq, Ord, Show, Read)
newtype SignalId = SignalId { fromSignalId :: String }
  deriving (Eq, Ord, Show, Read)

instance FromField BBPinId  where parseField = fmap BBPinId  . parseField
instance FromField MPUPinId where parseField = fmap MPUPinId . parseField
instance FromField SignalId where parseField = fmap SignalId . parseField

data BBPin = BBPin { bbPinId       :: BBPinId
                   , bbPinName     :: String
                   , bbPinMPUPinId :: Maybe MPUPinId
                   }
  deriving (Eq, Ord, Show, Read)

instance FromNamedRecord BBPin where
  parseNamedRecord v =
    BBPin <$> v .: "id"
          <*> v .: "name"
          <*> (unwrapMaybe <$> v .: "mpu_pin_id")

data MPUPin = MPUPin { mpuPinId        :: MPUPinId
                     , mpuPinLinuxName :: Maybe String
                     }
  deriving (Eq, Ord, Show, Read)

instance FromNamedRecord MPUPin where
  parseNamedRecord v =
    MPUPin <$> v .: "id"
           <*> (unwrapMaybe <$> v .: "linux_name")

data MPUPinSignal = MPUPinSignal { mpsMPUPinId :: MPUPinId
                                 , mpsMode     :: Maybe Integer
                                 , mpsSignalId :: SignalId
                                 }
  deriving (Eq, Ord, Show, Read)

instance FromNamedRecord MPUPinSignal where
  parseNamedRecord v =
    MPUPinSignal <$> v .: "mpu_pin_id"
                 <*> (unwrapMaybe <$> v .: "mode")
                 <*> v .: "signal_id"

data Signal = Signal { signalId           :: SignalId
                     , signalType         :: SignalType
                     , signalGPIONum      :: Maybe Integer
                     , signalLinuxPWMName :: Maybe String
                     }
  deriving (Eq, Ord, Show, Read)

instance FromNamedRecord Signal where
  parseNamedRecord v =
    Signal <$> v .: "id"
           <*> v .: "signal_type_id"
           <*> (unwrapMaybe <$> v .: "gpio_num")
           <*> (unwrapMaybe <$> v .: "linux_pwm_name")

data SignalType = A | I | O | IO | IOD | PWR | GND
  deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance FromField SignalType where
  parseField "A"   = pure A
  parseField "I"   = pure I
  parseField "O"   = pure O
  parseField "IO"  = pure IO
  parseField "IOD" = pure IOD
  parseField "PWR" = pure PWR
  parseField "GND" = pure GND
  parseField s     = fail ("Invalid SignalType: " ++ show s)

-- A wrapper for Maybe with a field parser that parses an empty string as
-- Nothing.

newtype WrapMaybe a = WrapMaybe { unwrapMaybe :: Maybe a }
  deriving (Eq, Ord, Show, Read)

instance FromField a => FromField (WrapMaybe a) where
  parseField "" = pure (WrapMaybe Nothing)
  parseField s  = WrapMaybe . Just <$> parseField s

-- Parsing.

parseData :: FilePath -> IO PinData
parseData dir = do
  bbPins         <- parseFile (dir </> "bb_pins.csv")
  mpuPins        <- parseFile (dir </> "mpu_pins.csv")
  mpuPinsSignals <- parseFile (dir </> "mpu_pins_signals.csv")
  signals        <- parseFile (dir </> "signals.csv")
  return (PinData bbPins mpuPins mpuPinsSignals signals)

parseFile :: FromNamedRecord a => FilePath -> IO [a]
parseFile path = do
  c <- BSL.readFile path
  (_header, vec) <- either fail return (decodeByName c)
  return (Vec.toList vec)
