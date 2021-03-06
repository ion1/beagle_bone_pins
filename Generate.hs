#!/usr/bin/env runhaskell

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

module Main (main) where

import qualified Generate.JSON    as JSON
import qualified Generate.Haskell    as Hs
import           Generate.SQL
import           System.Directory

main :: IO ()
main = do
  createDirectoryIfMissing True "gen/haskell/BeagleBone/Pins"
  withPinInfo $ \pinInfo -> do
    JSON.generate pinInfo "gen/beagle_bone_pins.json"
    Hs.generate   pinInfo "gen/haskell/BeagleBone/Pins/Data.hs"
