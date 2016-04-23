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
            [filename] -> do handle <- openFile filename ReadMode
                             mainloop filename handle initial_stats
                             hClose handle
            _ -> error "we need an input file name"

data Stats = Stats
  { stats_total::Int
  , stats_parsed::Int
  , stats_simple::Int
  , stats_column_histogram::Map.Map Int Int
  } deriving Show

initial_stats = Stats
  { stats_total = 0
  , stats_parsed = 0
  , stats_simple = 0
  , stats_column_histogram = Map.empty
  }

incr hist cols =  Map.insertWith (+) cols 1 hist

mainloop :: String -> Handle -> Stats -> IO ()
mainloop filename handle stats@(Stats { stats_total, stats_parsed, stats_simple, stats_column_histogram })  =
  hIsEOF handle >>=
  (\iseof -> if iseof
         then putStrLn ("parse errors / total queries  = " ++ show (stats_total - stats_parsed)  ++ " / " ++ show stats_total )
         else  (hGetLine handle
                >>= (\query ->
                      let (msg, newstats) =
                            let stats_updated_count =  stats { stats_total = stats_total + 1}
                            in case parseMySQLQuery filename (Just (stats_total + 1, 0)) query of
                              Left ParseError { peFormattedError } -> (peFormattedError, stats_updated_count )
                              Right _1 -> let stats_updated_parsed = stats_updated_count { stats_parsed = stats_parsed+1 }
                                          in if not (is_simple_scan _1) then (groom _1, stats_updated_parsed )
                                             else let cols =  distinct_columns_accessed $ get_all_idens _1
                                                  in (groom _1, stats_updated_parsed { stats_simple = stats_simple + 1
                                                                                     , stats_column_histogram = incr stats_column_histogram cols } )
                      in (putStrLn (show query)
                          >> putStrLn ""
                          >> putStrLn msg
                          >> putStrLn (groom stats)
                          >> putStrLn "---------"
                          >> mainloop filename handle newstats)
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

distinct_columns_accessed :: ColumnsUsed -> Int
distinct_columns_accessed ColumnsUsed {select_clause, where_clause} =
  Map.fromList (select_clause ++ where_clause) |> Map.toList |> length

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

note:
need to check for names that are nested within constructs not covered at the moment...
simple queries right now include queries like this one:
is_simple_scan  $ justParse "select count(distinct foo) from bar"

-}
