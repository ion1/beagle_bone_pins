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
{-# LANGUAGE RecordWildCards   #-}

module Generate.Types
( PinInfo (..)
, BBPin (..)
, MPUPin (..)
, MPUPinSignal (..)
, Signal (..)
) where

import           Control.Applicative
import           Data.Aeson
import qualified Data.Map            as Map
import           Data.Maybe          (catMaybes)

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
