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

module Generate.JSON (generate) where

import           Control.Applicative
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty   as P
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Char                  as Char
import           Data.Function              (on)
import qualified Data.Map                   as Map
import           Data.Monoid                (mconcat)
import qualified Data.Text                  as Text
import           Database.HDBC

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
  let res = Map.fromList [ ("bb_pins", toJSON bbPins)
                         , ("mpu_pins", toJSON mpuPins)
                         ]
  return (toJSON res)

genBBPins :: IConnection conn => conn -> IO (Map.Map String Value)
genBBPins conn =
  quickQuery conn "select id, name, mpu_pin_id from bb_pins" []
    >>= fmap Map.fromList . mapM fromDBRow
  where
    fromDBRow [pinId_, pinName_, mpuPinId_] =
      return (pinId, toJSON pinInfo)
      where
        pinInfo = mconcat
          [ Just Map.empty  -- Default to an empty map.
          , Map.singleton "name"    . toJSON <$> Just pinName
          , Map.singleton "mpu_pin" . toJSON <$> mpuPinId
          ]

        pinId    = fromSql pinId_    :: String
        pinName  = fromSql pinName_  :: String
        mpuPinId = fromSql mpuPinId_ :: Maybe String

    fromDBRow r = fail ("genBBPins: Unexpected DB row: " ++ show r)

genMPUPins :: IConnection conn => conn -> IO (Map.Map String Value)
genMPUPins conn =
  quickQuery conn "select id, linux_name from mpu_pins" []
    >>= fmap Map.fromList . mapM fromDBRow
  where
    fromDBRow [pinId_, pinLinuxName_] = do
      signalInfo <- genSignalInfo conn pinId
      let pinInfo = mconcat
            [ Just Map.empty  -- Default to an empty map.
            , Map.singleton "signals"    . toJSON <$> Just signalInfo
            , Map.singleton "linux_name" . toJSON <$> pinLinuxName
            ]
      return (pinId, toJSON pinInfo)
      where
        pinId        = fromSql pinId_        :: String
        pinLinuxName = fromSql pinLinuxName_ :: Maybe String

    fromDBRow r = fail ("genMPUPins: Unexpected DB row: " ++ show r)

genSignalInfo :: IConnection conn
              => conn -> String -> IO (Map.Map String Value)
genSignalInfo conn mpuPinId =
  quickQuery conn
    "select sig.id, pinsig.mode, typ.id, sig.gpio_num, sig.linux_pwm_name\
    \  from mpu_pins_signals pinsig\
    \  join signals sig on pinsig.signal_id = sig.id\
    \  join signal_types typ on sig.signal_type_id = typ.id\
    \  where pinsig.mpu_pin_id = ?"
    [SqlString mpuPinId]
    >>= fmap Map.fromList . mapM fromDBRow
  where
    fromDBRow [sigId_, mode_, typeId_, gpioNum_, linuxPWMName_] =
      return (sigId, toJSON sigInfo)
      where
        sigInfo = mconcat
          [ Just Map.empty  -- Default to an empty map.
          , Map.singleton "mode"           . toJSON <$> mode
          , Map.singleton "type"           . toJSON <$> Just typeId
          , Map.singleton "gpio_num"       . toJSON <$> gpioNum
          , Map.singleton "linux_pwm_name" . toJSON <$> linuxPWMName
          ]

        sigId        = fromSql sigId_        :: String
        mode         = fromSql mode_         :: Maybe Integer
        typeId       = fromSql typeId_       :: String
        gpioNum      = fromSql gpioNum_      :: Maybe Integer
        linuxPWMName = fromSql linuxPWMName_ :: Maybe String

    fromDBRow r = fail ("genSignalInfo: Unexpected DB row: " ++ show r)

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
