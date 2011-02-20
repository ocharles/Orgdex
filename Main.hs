import Orgdex
import System.Environment

index = "/home/ollie/Dropbox/gtd.org"

main = do
  contents <- readFile index
  database <- openDatabase "org.db"
  database `indexDocument` contents
  return ()

search query = do
  database <- openDatabase "org.db"
  database `findBlocks` query
