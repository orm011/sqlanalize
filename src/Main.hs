import System.Environment
import Language.SQL.SimpleSQL.Parse
import Language.SQL.SimpleSQL.Lex
import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.Pretty
import Language.SQL.SimpleSQL.Dialect
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Either()
import Text.Groom
import Debug.Trace
import qualified Data.Map.Strict as Map
import qualified Data.HashMap as HashMap
import qualified Data.Set as Set
import System.IO
import Data.Sexp
import GHC.Generics
import Language.Sexp
import Data.String.Utils
import qualified Data.ByteString.Lazy.Char8 as S8
import qualified Data.ByteString.Lazy as S
import Control.Parallel.Strategies

(|>) :: a -> (a -> b) -> b
(|>) f g = g f

main :: IO ()
main = do a <- getArgs
          case a of
            [filename] ->
              do contents <- readFile filename
                 let raw_queries = lines contents
                 let results = mainfun raw_queries
                 putStrLn (groom results { stats_tally = HashMap.empty })
                 display_cluster_tally (stats_tally results)
                 putStrLn (groom results { stats_tally = HashMap.empty })
            _ -> error "we need an input file name"

type Tally = HashMap.Map QueryExpr Int

data Stats = Stats
  { stats_total::Int
  , stats_error::Int
  , stats_tally::Tally
  } deriving (Show)

process_query :: String -> Either ParseError QueryExpr
process_query query_raw  =
  let query = replace_any_tweet_tokens query_raw in
  {- hack to correct some of the twitter queries -}
  case parseMySQLQuery "" Nothing query of
    Left _1 -> Left _1
    Right _1 -> Right $ normalize_query _1

replace_any_tweet_tokens :: String -> String
replace_any_tweet_tokens str = replace "ANY tweet_tokens" "ANY (tweet_tokens)" str

initial_stats = Stats
  { stats_total = 0
  , stats_error = 0
  , stats_tally = HashMap.empty
  }

incr hist cols =  Map.insertWith (+) cols 1 hist

merge_query :: Stats -> Either ParseError QueryExpr  -> Stats
merge_query s@Stats{ stats_total, stats_error } (Left _) =
  s {stats_total=stats_total+1
    ,stats_error=stats_error+1 }

merge_query s@Stats{ stats_total, stats_tally } (Right bs) =
  s {stats_total=stats_total+1
    ,stats_tally = HashMap.insertWith (+) bs 1 stats_tally}

mainfun :: [String] -> Stats
mainfun queries =
  let processed = parMap rpar process_query queries in
  foldl merge_query initial_stats processed

officialDialect =  Dialect {diSyntaxFlavour=MySQL, allowOdbc=True }
parseMySQLQuery = parseQueryExpr officialDialect
prettyMySQLQuery = prettyQueryExpr officialDialect

justParse str = case replace_any_tweet_tokens str |> parseMySQLQuery "" Nothing  of
  Right _1 -> _1

prettyParseErr file lineno query =
  let o  = parseMySQLQuery file (Just (lineno, 0)) query
  in case o of
    Left ParseError { peFormattedError } -> ( 1, peFormattedError )
    Right _1 -> ( 0, groom _1)

prettyParse str =
  prettyParseErr "" 0 str |> snd |> putStrLn

{- finds the identifiers used within an expression, possibly deep within -}
get_aliases :: QueryExpr -> [String]
get_aliases (Select { qeSelectList }) =
  map (Maybe.maybeToList . snd) qeSelectList |> foldl (++) [] |> map (\(Name _ s) -> s)

get_all_idens :: QueryExpr -> [String]
get_all_idens expr@(Select{}) =
  let initial = (get_all_idens_sexpr . toSexp) expr
      aliases = Set.fromList (get_aliases expr) in
  [i | i <- initial, not (Set.member i aliases )]

get_all_idens_sexpr :: Sexp -> [String]
get_all_idens_sexpr (List [Atom iden, _1@(List _) ])
  | (S8.unpack iden == "Iden") = [get_iden_list _1]
  | otherwise = get_all_idens_sexpr _1
get_all_idens_sexpr (Atom x) = []
get_all_idens_sexpr (List lst) =
  map get_all_idens_sexpr lst |> foldl (++) []

get_iden_list :: Sexp -> String
get_iden_list foo =
    let get_str (Name _ str) = str in
    case (fromSexp :: (Sexp -> Maybe [Name])) foo of
      Just namelist -> map get_str namelist |> foldl (\x y -> x ++"."++ y) "" |> tail

distinct_columns_accessed :: [String] -> Int
distinct_columns_accessed lst  = (Set.fromList lst) |> Set.size

{- get_canonical_query_sexpr  {-special case to deal with negative nums-}s@(List [Atom packed, second]) =-}

get_canonical_query_sexpr s@(List [Atom packed, second]) =
  if elem (S8.unpack packed) ["NumLit", "StringLit", "IntervalLit", "TypedLit"
                             , {-special case-} "PrefixOp"]
  then let mb = fromSexp s::Maybe ScalarExpr  in
    (mb |> Maybe.fromJust |> get_canonical_literal |> toSexp)
  else List [Atom packed, get_canonical_query_sexpr second]
get_canonical_query_sexpr s@(Atom x) = s
get_canonical_query_sexpr (List lst) =
  List (map get_canonical_query_sexpr lst)

{- also needs to deal with prefix op -}
get_canonical_literal :: ScalarExpr  -> ScalarExpr
get_canonical_literal s =
  let _1 = "{}" in
  case s of
    PrefixOp _ (NumLit _) -> NumLit "0" {- deal with annoying case of e.g -1 -}
    PrefixOp x inner -> let tmp = get_canonical_query_sexpr (toSexp inner)
                            inner' = Maybe.fromJust $ ((fromSexp tmp) :: Maybe ScalarExpr)
                        in PrefixOp x inner'
    NumLit _ -> NumLit "0"
    StringLit _ _ _ -> StringLit "'" "'" _1
    IntervalLit { ilLiteral } ->
      IntervalLit { ilSign=Nothing
                  , ilLiteral=ilLiteral
                  , ilFrom=(Itf _1 Nothing)
                  , ilTo=Nothing}
    TypedLit tn _ -> TypedLit tn _1


remove_neg_lits :: ScalarExpr -> ScalarExpr
remove_neg_lits (PrefixOp [Name Nothing "-"] (NumLit _1)) = NumLit ("-" ++ _1)

normalize_query :: QueryExpr -> QueryExpr
normalize_query q =
  q |> toSexp |> get_canonical_query_sexpr
  |> (fromSexp :: Sexp -> Maybe QueryExpr) |> Maybe.fromJust

display_cluster_tally :: Tally -> IO ()
display_cluster_tally m = {-note, it should be a one element list after parsing back -}
  (m
  |> HashMap.toList
  |> List.sortBy (\x y -> compare (snd x)  (snd y))
  |> map (\(x,y) -> (x, y, get_all_idens x |> distinct_columns_accessed))
  |> map (\(x, y, cols) -> putStrLn (prettyMySQLQuery x) >> putStrLn (" -> count: " ++ (show y) ++ " cols: " ++ (show cols)) >> putStrLn "---")
  |> sequence_)
  >> putStrLn ("number of unique queries: " ++ show (HashMap.size m))

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
