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

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Generate.JSON (generate) where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.Json.Builder
import qualified Data.Map                   as Map
import           Data.Monoid                ((<>))

import           Generate.Data

data Output = Output { outBBPins  :: Map.Map String OutBBPin
                     , outMPUPins :: Map.Map String OutMPUPin
                     }
  deriving (Eq, Ord, Show, Read)

instance Value Output where
  toJson (Output {..}) = toJson out
    where out = Map.fromList [ ("bb_pins",  toJson outBBPins)
                             , ("mpu_pins", toJson outMPUPins)
                             ]

data OutBBPin = OutBBPin { outBBPinName     :: String
                         , outBBPinMPUPinId :: Maybe String
                         }
  deriving (Eq, Ord, Show, Read)

instance Value OutBBPin where
  toJson (OutBBPin {..}) = toJson out
    where out =  Map.singleton "name" outBBPinName
              <> maybeToMap "mpu_pin" outBBPinMPUPinId

data OutMPUPin = OutMPUPin { outMPUPinLinuxName :: Maybe String
                           , outMPUPinSignals   :: Map.Map String OutSignal
                           }
  deriving (Eq, Ord, Show, Read)

instance Value OutMPUPin where
  toJson (OutMPUPin {..}) = toJson out
    where out =  (toJson <$> maybeToMap "linux_name" outMPUPinLinuxName)
              <> (toJson <$> Map.singleton "signals" outMPUPinSignals)

data OutSignal = OutSignal { outSignalMode         :: Maybe Integer
                           , outSignalType         :: String
                           , outSignalGPIONum      :: Maybe Integer
                           , outSignalLinuxPWMName :: Maybe String
                           }
  deriving (Eq, Ord, Show, Read)

instance Value OutSignal where
  toJson (OutSignal {..}) = toJson out
    where out =  (toJson <$> maybeToMap "mode" outSignalMode)
              <> (toJson <$> Map.singleton "type" outSignalType)
              <> (toJson <$> maybeToMap "gpio_num" outSignalGPIONum)
              <> (toJson <$> maybeToMap "linux_pwm_name" outSignalLinuxPWMName)

generate :: FilePath -> PinData -> IO ()
generate path pd = LBS8.writeFile path (toJsonLBS (output pd))

output :: PinData -> Output
output (PinData {..}) = Output bbPins mpuPins
  where
    bbPins = Map.fromList $ do
      BBPin {..} <- pdBBPins

      let outBBPin = OutBBPin bbPinName (fromMPUPinId <$> bbPinMPUPinId)

      return (fromBBPinId bbPinId, outBBPin)

    mpuPins = Map.fromList $ do
      MPUPin {..} <- pdMPUPins

      let outMPUPin = OutMPUPin mpuPinLinuxName (signals mpuPinId)

      return (fromMPUPinId mpuPinId, outMPUPin)

    signals mpuPinId = Map.fromList $ do
      MPUPinSignal {..} <- pdMPUPinsSignals
      guard (mpsMPUPinId == mpuPinId)

      Signal {..} <- pdSignals
      guard (signalId == mpsSignalId)

      let outSignal = OutSignal mpsMode
                                (show signalType)
                                signalGPIONum
                                signalLinuxPWMName

      return (fromSignalId signalId, outSignal)

maybeToMap :: k -> Maybe a -> Map.Map k a
maybeToMap _ Nothing    = Map.empty
maybeToMap k (Just val) = Map.singleton k val
