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

module Generate.Haskell (generate) where

import qualified Blaze.ByteString.Builder           as B
import qualified Blaze.ByteString.Builder.Char.Utf8 as B
import qualified Data.ByteString.Char8              as BS8
import qualified Data.Char                          as Char
import           Data.List
import qualified Data.Map                           as Map
import           Data.Monoid                        (mconcat, (<>))
import           System.IO                          (IOMode (..), withFile)

import           Generate.NumCompare
import           Generate.Types

class GenHs a where
  genHs :: a -> B.Builder

instance GenHs a => GenHs (Maybe a) where
  genHs = bMaybe genHs

instance (GenHs a) => GenHs [a] where
  genHs xs =  B.fromString "["
           <> mconcat ((intersperse (B.fromString ", ") . map genHs) xs)
           <> B.fromString "]"

instance (GenHs a, GenHs b) => GenHs (a,b) where
  genHs (a,b) =  B.fromString "("  <> genHs a
              <> B.fromString ", " <> genHs b
              <> B.fromString ")"

instance (GenHs a, GenHs b) => GenHs (Map.Map a b) where
  genHs theMap =  B.fromString "(Map.fromList "
               <> (genHs . Map.toList) theMap
               <> B.fromString ")"

instance GenHs BBPinId  where genHs = B.fromString . makeBBPinId
instance GenHs MPUPinId where genHs = B.fromString . makeMPUPinId
instance GenHs SignalId where genHs = B.fromString . makeSignalId
instance GenHs SignalType where genHs = B.fromShow

instance GenHs PinInfo where
  genHs (PinInfo {..}) =
    bUnlines
      [ genPinId "BBPinId"  ((map makeBBPinId  . Map.keys) piBBPins)
      , genPinId "MPUPinId" ((map makeMPUPinId . Map.keys) piMPUPins)
      , genPinId "SignalId" ((map makeSignalId . Map.keys) piSignals)
      , genBBPins  piBBPins
      , genMPUPins piMPUPins
      , genSignals piSignals
      ]

instance GenHs BBPin where
  genHs (BBPin {..}) =  B.fromString "(BBPin"
                     <> B.fromChar ' ' <> genHs bbpId
                     <> B.fromChar ' ' <> B.fromShow bbpName
                     <> B.fromChar ' ' <> genHs bbpMPUPinId
                     <> B.fromString ")"
instance GenHs MPUPin where
  genHs (MPUPin {..}) =  B.fromString "(MPUPin"
                      <> B.fromChar ' ' <> genHs mpId
                      <> B.fromChar ' ' <> bMaybe B.fromShow mpLinuxName
                      <> B.fromChar ' ' <> genHs mpSignals
                      <> B.fromString ")"
instance GenHs MPUPinSignal where
  genHs (MPUPinSignal {..}) =  B.fromString "(MPUPinSignal"
                            <> B.fromChar ' ' <> bMaybe B.fromShow mpsMode
                            <> B.fromChar ' ' <> genHs mpsSignalId
                            <> B.fromString ")"
instance GenHs Signal where
  genHs (Signal {..}) =  B.fromString "(Signal"
                      <> B.fromChar ' ' <> genHs sId
                      <> B.fromChar ' ' <> genHs sType
                      <> B.fromChar ' ' <> bMaybe B.fromShow sGPIONum
                      <> B.fromChar ' ' <> bMaybe B.fromShow sLinuxPWMName
                      <> B.fromString ")"

generate :: PinInfo -> FilePath -> IO ()
generate pinInfo path =
  withFile path WriteMode $ \f ->
    B.toByteStringIO (BS8.hPut f) builder
  where
    builder = bUnlines [ B.fromString header, genHs pinInfo ]

header :: String
header =
  unlines
    [ "-- Generated file. http://creativecommons.org/publicdomain/zero/1.0/"
    , ""
    , "module BeagleBone.Pins.Data"
    , "( BBPin (..)"
    , ", MPUPin (..)"
    , ", MPUPinSignal (..)"
    , ", Signal (..)"
    , ", SignalType (..)"
    , ", BBPinId (..)"
    , ", MPUPinId (..)"
    , ", SignalId (..)"
    , ", bbPins"
    , ", mpuPins"
    , ", signals"
    , ") where"
    , ""
    , "import qualified Data.Map as Map"
    , ""
    , "data BBPin = BBPin { bbpId       :: BBPinId"
    , "                   , bbpName     :: String"
    , "                   , bbpMPUPinId :: Maybe MPUPinId"
    , "                   }"
    , "  deriving (Eq, Ord, Show, Read)"
    , ""
    , "data MPUPin = MPUPin { mpId        :: MPUPinId"
    , "                     , mpLinuxName :: Maybe String"
    , "                     , mpSignals   :: Map.Map SignalId MPUPinSignal"
    , "                     }"
    , "  deriving (Eq, Ord, Show, Read)"
    , ""
    , "data MPUPinSignal = MPUPinSignal { mpsMode     :: Maybe Integer"
    , "                                 , mpsSignalId :: SignalId"
    , "                                 }"
    , "  deriving (Eq, Ord, Show, Read)"
    , ""
    , "data Signal = Signal { sId           :: SignalId"
    , "                     , sType         :: SignalType"
    , "                     , sGPIONum      :: Maybe Integer"
    , "                     , sLinuxPWMName :: Maybe String"
    , "                     }"
    , "  deriving (Eq, Ord, Show, Read)"
    , ""
    , "data SignalType = A | I | O | IO | IOD | PWR | GND"
    , "  deriving (Eq, Ord, Bounded, Enum, Show, Read)"
    ]

genPinId :: String -> [String] -> B.Builder
genPinId name pinIds =
  bUnlines [ mconcat [ B.fromString "data ", B.fromString name
                     , B.fromString " = "
                     , B.fromString (intercalate " | " pinIdsSorted)
                     ]
           , B.fromString "  deriving (Eq, Ord, Bounded, Enum, Show, Read)"
           ]
  where
    pinIdsSorted = sortBy numCompareString pinIds

genBBPins :: Map.Map BBPinId BBPin -> B.Builder
genBBPins bbPins =
  bUnlines [ B.fromString "bbPins :: Map.Map BBPinId BBPin"
           , B.fromString "bbPins = " <> genHs bbPins
           ]

genMPUPins :: Map.Map MPUPinId MPUPin -> B.Builder
genMPUPins mpuPins =
  bUnlines [ B.fromString "mpuPins :: Map.Map MPUPinId MPUPin"
           , B.fromString "mpuPins = " <> genHs mpuPins
           ]

genSignals :: Map.Map SignalId Signal -> B.Builder
genSignals signals =
  bUnlines [ B.fromString "signals :: Map.Map SignalId Signal"
           , B.fromString "signals = " <> genHs signals
           ]

makeBBPinId :: BBPinId -> String
makeBBPinId name = validate str `seq` str
  where str = "BB_" ++ fromBBPinId name

makeMPUPinId :: MPUPinId -> String
makeMPUPinId name = validate str `seq` str
  where str = "MPU_" ++ fromMPUPinId name

makeSignalId :: SignalId -> String
makeSignalId name = validate str `seq` str
  where str = "Sig_" ++ fromSignalId name

validate :: String -> ()
validate name
  | all validChar name = ()
  | otherwise = error ("Invalid name: " ++ show name)
  where
    validChar '_' = True
    validChar c   = Char.isAlphaNum c

bMaybe :: (a -> B.Builder) -> Maybe a -> B.Builder
bMaybe f (Just a) = B.fromString "(Just " <> f a <> B.fromString ")"
bMaybe _ Nothing  = B.fromString "Nothing"

bUnlines :: [B.Builder] -> B.Builder
bUnlines = mconcat . map (<> B.fromChar '\n')
