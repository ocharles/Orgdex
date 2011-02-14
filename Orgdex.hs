-- Orgdex - an indexer for org-mode files

import Control.Applicative hiding (many, optional, (<|>))
import Control.Monad
import Data.Char (isSpace)
import Text.ParserCombinators.Parsec

data Node = Node { level :: Int, keyword :: Maybe String, heading :: String, tags :: Maybe [String] }
  deriving (Show)

parseNode = Node <$> level <*> (optionMaybe keyword) <*> name <*> (optionMaybe tags)
    where level = length <$> many1 (char '*') <* space
          keyword = (try (many1 upper <* space))
          name = noneOf "\n" `manyTill` (eof <|> (lookAhead (try (tags *> eof))))
          tags = char ':' *> many1 alphaNum `sepEndBy1` char ':'

myTest = parse parseNode "org-mode" "** Some : text here :tags: JUST KIDDING :tags:here:"
myTest2 = parse parseNode "org-mode" "* TODO Just a node"
