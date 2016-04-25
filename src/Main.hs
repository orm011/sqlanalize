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
import qualified Data.Set as Set
import System.IO
import Data.Sexp
import GHC.Generics
import Data.String.Conversions
import Language.Sexp
import qualified Data.ByteString.Lazy.Char8 as S8
import qualified Data.ByteString.Lazy as S

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

{- finds the identifiers used within an expression, possibly deep within -}
get_all_idens :: QueryExpr -> [String]
get_all_idens expr@(Select{}) = (get_all_idens_sexpr . toSexp) expr

get_all_idens_sexpr :: Sexp -> [String]
get_all_idens_sexpr (List [Atom iden, _1@(List _) ])
  | (S8.unpack iden == "Iden") = [get_iden_list _1]
  | otherwise = get_all_idens_sexpr _1
get_all_idens_sexpr (Atom x) = []
get_all_idens_sexpr (List lst) =
  map get_all_idens_sexpr lst |> foldl (++) []

get_iden_list :: Sexp -> String
get_iden_list foo =
    let get_str (Name Nothing str) = str in
    case (fromSexp :: (Sexp -> Maybe [Name])) foo of
      Just namelist -> map get_str namelist |> foldl (\x y -> x ++"."++ y) "" |> tail

distinct_columns_accessed :: [String] -> Int
distinct_columns_accessed lst  = (Set.fromList lst) |> Set.size

{-
note:
simple queries right now include queries like this one:
is_simple_scan  $ justParse "select count(distinct foo) from bar"

also, we are having issues parsing queries with 'any Fooo'
-}

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
