{- based on intro to parsing -}

import System.Environment
{- import Text.Parsec (ParseError, errorPos, sourceName, sourceLine, sourceColumn
-- import Text.Parsec.String -}
import Language.SQL.SimpleSQL.Parser
import Data.List

main :: IO ()
main = let qexp = parseQueryExprs "" Nothing "select 1; select 2"
       in putStr $ show qexp

-- main = do
--     a <- getArgs
--     case a of
--       [str] -parseFromFile myParser str
--                >>= either (putStrLn . showError)
--                           (putStrLn . intercalate "\n" . map P.prettyQueryExpr)
--       _ -error "please pass one argument with the file containing the queries to parse"

-- myParser :: Parser [QueryExpr]
-- myParser = whitespace
--            *sepBy1 queryExpr semi
--            <* optional semi
--            <* eof
--   where semi = void $ lexeme $ char ';'

-- showError :: ParseError -String
-- showError e =
--     let p = errorPos e
--     in sourceName p ++ ":" ++ show (sourceLine p) ++ ":"
--        ++ show (sourceColumn p) ++ ":\n"
--        ++ show e
