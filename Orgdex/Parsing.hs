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

-- | Org-mode parser
module Orgdex.Parsing
       (
         parseOrgDocument
       , OrgDocument (OrgDocument)
       , DocumentPart (Heading, Paragraph)
       ) where

import Control.Applicative hiding (many, optional, (<|>))
import Control.Monad
import Data.Char (isSpace)
import Text.ParserCombinators.Parsec

instance Applicative (GenParser s a) where
    pure  = return
    (<*>) = ap

data DocumentPart = Heading
                      {
                        level :: Int
                      , keyword :: Maybe String
                      , heading :: String
                      , tags :: [String]
                      }
                  | Paragraph String
                  deriving (Show)

data OrgDocument = OrgDocument [DocumentPart]
  deriving (Show)

-- Parse an org-mode document
parseTopLevel = do 
  blanklines
  blocks <- manyTill parseBlock eof
  return (OrgDocument blocks)

parseBlock = choice [ parseHeading, parseParagraph ]

-- Parse an org-mode heading
-- Headings are
--    - 1 or more *s indicating the level of the heading
--    - a keyword (ie TODO)
--    - the heading name itself
--    - a list of tags (between :, separated by :, eg :foo:bar:)
parseHeading =
  Heading <$> level <*> optionMaybe keyword <*> name <*> option [] tags <* blanklines
    where level = length <$> many1 (char '*') <* space
          keyword = try $ many1 upper <* space
          name = noneOf "\n" `manyTill` lookAhead (try $ (tags *> eol) <|> eol)
          tags = char ':' *> many1 alphaNum `sepEndBy1` char ':'

parseParagraph =
  do text <- anyChar `manyTill` (try $ newline >> blanklines)
     return (Paragraph text)

blanklines = many1 blankline
  where blankline = try $ skipSpaces >> newline
        skipSpaces = skipMany spaceChar
        spaceChar = satisfy $ \c -> c == ' '

-- Make sure we're at the end of a line, where the end of the stream will also count
-- Discard the result.
eol = (char '\n' >> return ()) <|> eof

parseOrgDocument input = case parse parseTopLevel "org-mode" input of
  (Right parsed) -> parsed
  (Left failure) -> error $ show failure
