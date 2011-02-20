{-
Copyright (C) 2011 by Oliver Charles <oliver.g.charles@googlemail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN

-}

-- | Orgdex, an org-mode document indexer
module Orgdex
       (
         parseOrgDocument
       , openDatabase
       , indexDocument
       , findBlocks
       ) where

import Control.Monad (mapM, zipWithM)
import Orgdex.Parsing
import Search.Xapian hiding ( (<|>), Database, openDatabase )
import qualified Search.Xapian as Xapian
import Text.ParserCombinators.Parsec

newtype Database = Database { getDatabase :: Xapian.Database }

openDatabase path =
  do (Right db) <- openWritableDatabase path createOrOpenDB
     return (Database db)

documentTerms :: OrgDocument -> [String]
documentTerms (OrgDocument parts) = concat $ blockTerms `map` parts

blockTerms :: DocumentPart -> [String]
blockTerms (Heading _ keyword name tags) =
  keywordTerms
  where tagTerms = ("T"++) `map` tags
        nameTerms = []
        keywordTerms = case keyword of
          (Just keyword) -> ["K" ++ keyword]
          _ -> []
blockTerms _ = []

indexBlock database (Heading _ keyword name tags) =
  do doc <- newDocument
     doc `setDocumentData` name
     zipWithM (addPosting doc) tags [1..]
     stemToDocument englishStem doc name
     database `addDocument` doc
     return ()

indexDocument database document =
  (indexBlock $ getDatabase database) `mapM` blocks
  where blocks = case parseOrgDocument document of
          (OrgDocument parts) -> parts

findBlocks database queryStr =
  do let query = parseQuery englishStem queryStr
     let db = getDatabase database
     documentIds <- enquire db query 0 100
     documents <- (getDocument db) `mapM` documentIds
     (fmap parseOrgDocument) `fmap` (getDocumentData `mapM` documents)
