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

import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty   as P
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Char                  as Char
import           Data.Function              (on)
import qualified Data.Text                  as Text

import           Generate.Types

generate :: PinInfo -> FilePath -> IO ()
generate pinInfo path = LBS8.writeFile path encoded
  where
    encoded = P.encodePretty' conf (toJSON pinInfo)

    conf = P.Config { P.confIndent  = 2
                    , P.confCompare = Just numCompare
                    }

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
