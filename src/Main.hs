import System.Environment
import Language.SQL.SimpleSQL.Parse
import Language.SQL.SimpleSQL.Lex
import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.Pretty
import Language.SQL.SimpleSQL.Dialect
import Data.List
import Data.Maybe
import Data.Either
import Text.Groom
import Text.Regex.Posix
import Debug.Trace
import qualified Data.Map.Strict as Map
import System.IO

(|>) :: a -> (a -> b) -> b
(|>) f g = g f

main :: IO ()
main = do a <- getArgs
          case a of
            [str] -> do handle <- openFile str ReadMode
                        mainloop str handle 0 0
                        hClose handle
            _ -> error "we need an input file name"

mainloop :: String -> Handle -> Int -> Int -> IO ()
mainloop filename handle lineno errno  =
  hIsEOF handle >>=
  (\iseof -> if iseof
         then putStrLn ("parse errors / total queries  = " ++ show errno  ++ " / " ++ show (lineno + 1) )
         else  (hGetLine handle
                >>= (\query ->
                      let (delta_err, o)  = prettyParseErr filename lineno query
                      in (putStrLn (show query)
                          >> putStrLn ""
                          >> putStrLn o
                          >> putStrLn "---------"
                          >> mainloop filename handle (lineno+1) (errno+delta_err))
                    )
               )
  )

-- remove_limit :: String -> String
-- remove_limit query =
--   case (query  =~ "(limit|LIMIT) ([0-9]*);" :: (String, String, String, [String])) of
--     (before, _, _, [_, num]) ->  before ++ "offset 0 fetch first " ++ num ++ " rows only;"
--     (before, "", "", [] ) -> before

officialDialect =  Dialect {diSyntaxFlavour=MySQL, allowOdbc=True }
parseMySQLQuery = parseQueryExpr officialDialect
prettyMySQLQuery = prettyQueryExpr officialDialect

justParse str = case parseMySQLQuery "" Nothing str of
  Right _1 -> _1

prettyParseErr file lineno query =
  let o  = parseMySQLQuery file (Just (lineno, 0)) query
  in case o of
    Left ParseError { peFormattedError } -> ( 1, peFormattedError )
    Right _1 -> ( 0, groom _1)

prettyParse str =
  prettyParseErr "" 0 str |> snd |> putStrLn

extract_name (Name _ x) = x
{- finds the identifiers used within an expression, possibly deep within -}
get_idens :: ScalarExpr -> [String]
get_idens (Iden ls) = [intercalate "." (map extract_name ls)]
get_idens Star = ["star"] {- fix later by using schema -}
get_idens (App _ vals) = concat $ map get_idens vals
get_idens (AggregateApp { aggArgs, aggFilter }) =
  concat $ (map get_idens aggArgs) ++ (map get_idens (maybeToList aggFilter))
get_idens (BinOp _1  _  _2) = get_idens _1 ++ get_idens _2
get_idens (PrefixOp _ _1) = get_idens _1
get_idens (Parens x) = get_idens x
get_idens _ = []

data ColumnsUsed = ColumnsUsed { select_clause :: [(String, Int)]
                               , where_clause :: [(String, Int)]
                               } deriving Show

group_by_sum :: [String] -> [(String, Int)]
group_by_sum = Map.toList . (foldl (\map elt -> Map.insertWith (+) elt 1 map) Map.empty)

get_all_idens :: QueryExpr -> ColumnsUsed
get_all_idens Select { qeSelectList,  qeWhere  } =
  ColumnsUsed { select_clause = group_by_sum $ concat $ (map (get_idens . fst) qeSelectList)
              , where_clause = group_by_sum $ concat $ (map get_idens (maybeToList qeWhere)) }

{- select (agg) from single_table where -}
is_simple_scan :: QueryExpr -> Bool
is_simple_scan Select {
  qeSetQuantifier = SQDefault
  , qeFrom = [TRSimple [Name _ _]]
  , qeGroupBy = []
  , qeHaving = Nothing
  , qeOrderBy = []
  , qeOffset = Nothing
  , qeFetchFirst = Nothing
  } = True
is_simple_scan _ = False

{-
is_simple_scan
is_join
is_aggregate
-}
