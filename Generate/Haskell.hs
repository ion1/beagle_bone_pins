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

import           Control.Arrow                (first)
import qualified Data.Char                    as Char
import           Data.Function                (on)
import           Data.List                    (sortBy)
import qualified Data.Map                     as Map
import           Language.Haskell.Exts
import           Language.Haskell.Exts.SrcLoc (noLoc)
import           System.IO                    (IOMode (..), hPutStr, withFile)

import           Generate.NumCompare
import           Generate.Types

generate :: PinInfo -> FilePath -> IO ()
generate pinInfo path =
  withFile path WriteMode $ \f ->
    hPutStr f code
  where
    code = unlines
      [ "-- Generated file. http://creativecommons.org/publicdomain/zero/1.0/"
      , ""
      , prettyPrint ast
      ]

    ast = case parseModule header of
      ParseOk (Module loc modName pragmas warns exports imports decls) ->
        Module loc modName pragmas warns exports imports (decls ++ newDecls)
      other -> error ("Failed to parse header: " ++ show other)

    newDecls = genPinInfo pinInfo

header :: String
header =
  unlines
    [ "module BeagleBone.Pins.Data"
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

genPinInfo :: PinInfo -> [Decl]
genPinInfo (PinInfo {..}) =
  concat
    [ genPinId "BBPinId"  ((map makeBBPinId  . Map.keys) piBBPins)
    , genPinId "MPUPinId" ((map makeMPUPinId . Map.keys) piMPUPins)
    , genPinId "SignalId" ((map makeSignalId . Map.keys) piSignals)
    , genBBPins  piBBPins
    , genMPUPins piMPUPins
    , genSignals piSignals
    ]

genPinId :: String -> [String] -> [Decl]
genPinId dataName pinIds =
  [ DataDecl noLoc DataType [] (name dataName) [] pinCons derivs ]
  where
    pinCons = map pinCon pinIdsSorted
    pinIdsSorted = sortBy numCompareString pinIds
    pinCon conId = QualConDecl noLoc [] [] (ConDecl (name conId) [])

    derivs = map (\n -> (UnQual (name n), []))
                 ["Eq", "Ord", "Bounded", "Enum", "Show", "Read"]

genBBPins :: Map.Map BBPinId BBPin -> [Decl]
genBBPins bbPins =
  genMap "bbPins" "BBPin" "BBPinId"
         ((map gen . mapToListSorted makeBBPinId) bbPins)
  where
    gen (mpIdStr, BBPin {..}) =
      Tuple [ (con . name) mpIdStr
            , appFun ((con . name) "BBPin")
                     [ (con . name) mpIdStr
                     , strE bbpName
                     , astMaybe (con . name . makeMPUPinId) bbpMPUPinId
                     ]
            ]

genMPUPins :: Map.Map MPUPinId MPUPin -> [Decl]
genMPUPins mpuPins =
  genMap "mpuPins" "MPUPin" "MPUPinId"
         ((map gen . mapToListSorted makeMPUPinId) mpuPins)
  where
    gen (mpIdStr, MPUPin {..}) =
      Tuple [ (con . name) mpIdStr
            , appFun ((con . name) "MPUPin")
                     [ (con . name) mpIdStr
                     , astMaybe strE mpLinuxName
                     , genMPUPinSignals mpSignals
                     ]
            ]

genMPUPinSignals :: Map.Map SignalId MPUPinSignal -> Exp
genMPUPinSignals mpSignals =
  app (qvar (ModuleName "Map") (name "fromList"))
      (listE ((map gen . mapToListSorted makeSignalId) mpSignals))
  where
    gen (sIdStr, MPUPinSignal {..}) =
      Tuple [ (con . name) sIdStr
            , appFun ((con . name) "MPUPinSignal")
                     [ astMaybe intE mpsMode
                     , (con . name) sIdStr
                     ]
            ]

genSignals :: Map.Map SignalId Signal -> [Decl]
genSignals signals =
  genMap "signals" "Signal" "SignalId"
         ((map gen . mapToListSorted makeSignalId) signals)
  where
    gen (sIdStr, Signal {..}) =
      Tuple [ (con . name) sIdStr
            , appFun ((con . name) "Signal")
                     [ (con . name) sIdStr
                     , (con . name . show) sType
                     , astMaybe intE sGPIONum
                     , astMaybe strE sLinuxPWMName
                     ]
            ]

genMap :: String -> String -> String -> [Exp] -> [Decl]
genMap valName typeName idName valueList =
  [ TypeSig noLoc [name valName] typeAST
  , PatBind noLoc (pvar (name valName)) Nothing (UnGuardedRhs valueAST)
            (BDecls [])
  ]
  where
    typeAST    =    qtycon (ModuleName "Map") (name "Map")
            `TyApp` tycon (name idName)
            `TyApp` tycon (name typeName)

    valueAST = app (qvar (ModuleName "Map") (name "fromList"))
                   (listE valueList)

mapToListSorted :: (k -> String) -> Map.Map k v -> [(String, v)]
mapToListSorted f = sortBy (numCompareString `on` fst)
                  . map (first f)
                  . Map.toList

makeBBPinId :: BBPinId -> String
makeBBPinId origId = validate str `seq` str
  where str = "BB_" ++ fromBBPinId origId

makeMPUPinId :: MPUPinId -> String
makeMPUPinId origId = validate str `seq` str
  where str = "MPU_" ++ fromMPUPinId origId

makeSignalId :: SignalId -> String
makeSignalId origId = validate str `seq` str
  where str = "Sig_" ++ fromSignalId origId

validate :: String -> ()
validate valName
  | all validChar valName = ()
  | otherwise = error ("Invalid name: " ++ show valName)
  where
    validChar '_' = True
    validChar c   = Char.isAlphaNum c

-- Some haskell-src-exts helpers.

astMaybe :: (a -> Exp) -> Maybe a -> Exp
astMaybe f (Just a) = app ((con . name) "Just") (f a)
astMaybe _ Nothing  = (con . name) "Nothing"

tycon :: Name -> Type
tycon n    = TyCon (UnQual n)

qtycon :: ModuleName -> Name -> Type
qtycon m n = TyCon (Qual m n)

con :: Name -> Exp
con n      = Con (UnQual n)

--qcon :: ModuleName -> Name -> Exp
--qcon m n   = Con (Qual m n)
