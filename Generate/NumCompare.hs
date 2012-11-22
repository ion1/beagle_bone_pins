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


module Generate.NumCompare
( numCompareString
, numCompareText
, numCompareTextL
, numCompareBS
, numCompareBSL
, numCompare
) where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Char                  as Char
import           Data.Function              (on)
import qualified Data.List                  as List
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as TextL

data NumSort a = NSNumber Integer
               | NSOther a
  deriving (Eq, Ord, Show, Read)

numCompareString :: String         -> String         -> Ordering
numCompareText   :: Text.Text      -> Text.Text      -> Ordering
numCompareTextL  :: TextL.Text     -> TextL.Text     -> Ordering
numCompareBS     :: BS.ByteString  -> BS.ByteString  -> Ordering
numCompareBSL    :: BSL.ByteString -> BSL.ByteString -> Ordering

numCompareString = numCompare List.groupBy uncons id
  where
    uncons (x:xs) = Just (x, xs)
    uncons _      = Nothing

numCompareText  = numCompare Text.groupBy  Text.uncons  Text.unpack
numCompareTextL = numCompare TextL.groupBy TextL.uncons TextL.unpack
numCompareBS    = numCompare BS8.groupBy   BS8.uncons   BS8.unpack
numCompareBSL   = numCompare BSL8.groupBy  BSL8.uncons  BSL8.unpack

numCompare :: Ord a
           => ((Char -> Char -> Bool) -> a -> [a])  -- ^ groupBy
           -> (a -> Maybe (Char, a))                -- ^ uncons
           -> (a -> String)                         -- ^ toString
           -> a -> a -> Ordering
numCompare groupBy uncons toString = compare `on` map itemToNumSort . grouped
  where
    grouped = groupBy ((==) `on` Char.isDigit)

    itemToNumSort xs =
      case uncons xs of
        Just (c, _) | Char.isDigit c ->
          case reads (toString xs) of
            [(n, [])] -> NSNumber n
            _         -> NSOther xs

        _ -> NSOther xs
