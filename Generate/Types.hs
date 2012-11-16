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

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Generate.Types
( BBPinId (..)
, MPUPinId (..)
, SignalId (..)
, PinInfo (..)
, BBPin (..)
, MPUPin (..)
, MPUPinSignal (..)
, Signal (..)
) where

import           Control.Applicative
import           Data.Aeson
import           Data.Convertible
import qualified Data.Map            as Map
import           Data.Maybe          (catMaybes)
import           Data.Typeable       (Typeable)
import           Database.HDBC       (SqlValue (..))

newtype BBPinId = BBPinId { fromBBPinId :: String }
  deriving (Eq, Ord, Show, Read, Typeable)
newtype MPUPinId = MPUPinId { fromMPUPinId :: String }
  deriving (Eq, Ord, Show, Read, Typeable)
newtype SignalId = SignalId { fromSignalId :: String }
  deriving (Eq, Ord, Show, Read, Typeable)

data PinInfo = PinInfo { piBBPins  :: Map.Map BBPinId  BBPin
                       , piMPUPins :: Map.Map MPUPinId MPUPin
                       , piSignals :: Map.Map SignalId Signal
                       }
  deriving (Eq, Ord, Show, Read, Typeable)

data BBPin = BBPin { bbpId       :: BBPinId
                   , bbpName     :: String
                   , bbpMPUPinId :: Maybe MPUPinId
                   }
  deriving (Eq, Ord, Show, Read, Typeable)

data MPUPin = MPUPin { mpId        :: MPUPinId
                     , mpLinuxName :: Maybe String
                     , mpSignals   :: Map.Map SignalId MPUPinSignal
                     }
  deriving (Eq, Ord, Show, Read, Typeable)

data MPUPinSignal = MPUPinSignal { mpsMode     :: Maybe Integer
                                 , mpsSignalId :: SignalId
                                 }
  deriving (Eq, Ord, Show, Read, Typeable)

data Signal = Signal { sId           :: SignalId
                     , sType         :: SignalType
                     , sGPIONum      :: Maybe Integer
                     , sLinuxPWMName :: Maybe String
                     }
  deriving (Eq, Ord, Show, Read, Typeable)

data SignalType = A | I | O | IO | IOD | PWR | GND
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Typeable)

instance ToJSON BBPinId  where toJSON = toJSON . fromBBPinId
instance ToJSON MPUPinId where toJSON = toJSON . fromMPUPinId
instance ToJSON SignalId where toJSON = toJSON . fromSignalId

instance Convertible SqlValue BBPinId where
  safeConvert = fmap BBPinId . safeConvert
instance Convertible SqlValue MPUPinId where
  safeConvert = fmap MPUPinId . safeConvert
instance Convertible SqlValue SignalId where
  safeConvert = fmap SignalId . safeConvert

instance ToJSON PinInfo where
  toJSON (PinInfo {..}) =
    object [ "bb_pins"  .= Map.mapKeys fromBBPinId  piBBPins
           , "mpu_pins" .= Map.mapKeys fromMPUPinId piMPUPins
           , "signals"  .= Map.mapKeys fromSignalId piSignals
           ]

instance ToJSON BBPin where
  toJSON (BBPin {..}) =
    (object . catMaybes) [ ("name"    .=) <$> Just bbpName
                         , ("mpu_pin" .=) <$> bbpMPUPinId
                         ]

instance ToJSON MPUPin where
  toJSON (MPUPin {..}) =
    (object . catMaybes) [ ("linux_name" .=) <$> mpLinuxName
                         , ("signals"    .=) <$> Just mpSignals'
                         ]
    where mpSignals' = Map.mapKeys fromSignalId mpSignals

instance ToJSON MPUPinSignal where
  toJSON (MPUPinSignal {..}) =
    (object . catMaybes) [ ("mode" .=) <$> mpsMode ]

instance ToJSON Signal where
  toJSON (Signal {..}) =
    (object . catMaybes) [ ("type"           .=) <$> Just sType
                         , ("gpio_num"       .=) <$> sGPIONum
                         , ("linux_pwm_name" .=) <$> sLinuxPWMName
                         ]

instance ToJSON SignalType where toJSON = toJSON . show

instance Convertible SqlValue SignalType where
  safeConvert sqlv = readConvert =<< safeConvert sqlv
    where
      readConvert s =
        case reads s of
          [(st, [])] -> return st
          r -> convError ("reads " ++ show s ++ " returned: " ++ show r) sqlv
