-- Orgdex - an indexer for org-mode files

import Control.Applicative hiding (many, optional, (<|>))
import Control.Monad
import Data.Char (isSpace)
import Text.ParserCombinators.Parsec

data Heading = Heading { level :: Int, keyword :: Maybe String, heading :: String, tags :: Maybe [String] }
  deriving (Show)

data OrgDocument = OrgDocument [Heading]
  deriving (Show)

parseHeading = Heading <$> level <*> (optionMaybe keyword) <*> name <*> (optionMaybe tags)
    where level = length <$> many1 (char '*') <* space
          keyword = try $ many1 upper <* space
          name = noneOf "\n" `manyTill` ((newline >> return ()) <|> eof <|> (lookAhead $ try $ tags *> eof))
          tags = char ':' *> many1 alphaNum `sepEndBy1` char ':'

parseOrgDocument = many parseHeading

parseOrg input = parse parseOrgDocument "org-mode" input

myTest = parseOrg "** Some : text here :tags: JUST KIDDING :tags:here:\n* Heading"
myTest2 = parseOrg "* TODO Just a node\n* Test"
