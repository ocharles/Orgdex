-- Orgdex - an indexer for org-mode files

import Control.Applicative hiding (many, optional, (<|>))
import Control.Monad
import Data.Char (isSpace)
import Text.ParserCombinators.Parsec

data DocumentPart = Heading {
      level :: Int,
      keyword :: Maybe String,
      heading :: String,
      tags :: [String]
    } deriving (Show)

data OrgDocument = OrgDocument [DocumentPart]
  deriving (Show)

-- Parse an org-mode document
parseOrgDocument = many $ parseHeading

-- Parse an org-mode heading
-- Headings are
--    - 1 or more *s indicating the level of the heading
--    - a keyword (ie TODO)
--    - the heading name itself
--    - a list of tags (between :, separated by :, eg :foo:bar:)
parseHeading =
    Heading <$> level <*> (optionMaybe keyword) <*> name <*> (option [] tags) <* eol
    where level = length <$> many1 (char '*') <* space
          keyword = try $ many1 upper <* space
          name = noneOf "\n" `manyTill` (lookAhead (try ((tags *> eol) <|> eol)))
          tags = char ':' *> many1 alphaNum `sepEndBy1` char ':'

-- Make sure we're at the end of a line, where the end of the stream will also count
-- Discard the result.
eol = (char '\n' >> return ()) <|> eof

parseOrg input = parse parseOrgDocument "org-mode" input

myTest = parseOrg "** Some : text here :tags: JUST KIDDING :tags:here:\n* Heading"
myTest2 = parseOrg "* TODO Just a node\n* Test"
