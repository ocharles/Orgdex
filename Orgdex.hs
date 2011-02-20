-- Orgdex - an indexer for org-mode files

import Control.Applicative hiding (many, optional, (<|>))
import Control.Monad
import Data.Char (isSpace)
import Text.ParserCombinators.Parsec

instance Applicative (GenParser s a) where
    pure  = return
    (<*>) = ap

data DocumentPart = Heading Int (Maybe String) String [String]
                  | Paragraph String
                  deriving (Show)

data OrgDocument = OrgDocument [DocumentPart]
  deriving (Show)

-- Parse an org-mode document
parseOrgDocument = blanklines >> manyTill block eof

block = choice [ heading, paragraph ]

-- Parse an org-mode heading
-- Headings are
--    - 1 or more *s indicating the level of the heading
--    - a keyword (ie TODO)
--    - the heading name itself
--    - a list of tags (between :, separated by :, eg :foo:bar:)
heading =
  Heading <$> level <*> optionMaybe keyword <*> name <*> option [] tags <* blanklines
    where level = length <$> many1 (char '*') <* space
          keyword = try $ many1 upper <* space
          name = noneOf "\n" `manyTill` lookAhead (try $ (tags *> eol) <|> eol)
          tags = char ':' *> many1 alphaNum `sepEndBy1` char ':'

paragraph =
  do text <- anyChar `manyTill` (try $ newline >> blanklines)
     return (Paragraph text)

blanklines = many1 blankline
  where blankline = try $ skipSpaces >> newline
        skipSpaces = skipMany spaceChar
        spaceChar = satisfy $ \c -> c == ' '

-- Make sure we're at the end of a line, where the end of the stream will also count
-- Discard the result.
eol = (char '\n' >> return ()) <|> eof

parseOrg input = parse parseOrgDocument "org-mode" input

myTest = parseOrg "** Some : text here :tags: JUST KIDDING :tags:here:\n* Heading"
myTest2 = parseOrg "* TODO Just a node\n* Test"
