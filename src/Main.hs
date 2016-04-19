import System.Environment
import Language.SQL.SimpleSQL.Parser
import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.Pretty
import Data.List
import Data.Maybe
import Data.Either
import Text.Groom

main :: IO ()
main = do a <- getArgs
          case a of
            [str] -> do f <- readFile str
                        process_lines str $ lines f
            _ -> error "we need an input file name"

process_lines :: String -> [String] -> IO ()
process_lines str lns =
  let parsed  = partitionEithers $ map (parseQueryExpr str Nothing)  lns
      num_errors = length $ fst  parsed
      total_lines = (length $ snd parsed) + num_errors
      good_ones = snd $ parsed
      idens = map get_all_idens good_ones
      rows = transpose [ map prettyQueryExpr good_ones
                       , map show idens
                       , map groom good_ones]
      disp [x,y,z] = putStrLn x >> putStrLn "" >>
        putStrLn y >> putStrLn "" >>
        putStrLn z >> putStrLn "---------"
      msg = "parse errors / total queries  = " ++
        show num_errors ++ " / " ++ show total_lines
      actions = (map disp rows) ++ [putStrLn $ msg]
  in sequence_ actions

extract_name (Name x) = x
{- finds the identifiers used within an expression, possibly deep within -}
get_idens :: ValueExpr -> [String]
get_idens (Iden ls) = [intercalate "." (map extract_name ls)]
get_idens Star = ["star"] {- fix later by using schema -}
get_idens (App _ vals) = concat $ map get_idens vals
get_idens (AggregateApp { aggArgs, aggFilter }) =
  concat $ (map get_idens aggArgs) ++ (map get_idens (maybeToList aggFilter))
get_idens (BinOp _1  _  _2) = get_idens _1 ++ get_idens _2
get_idens (Parens x) = get_idens x
get_idens _ = []

get_all_idens :: QueryExpr -> [String]
get_all_idens Select { qeSelectList, qeWhere  } =
  concat $ (map (get_idens . fst) qeSelectList) ++ (map get_idens (maybeToList qeWhere))
get_all_idens  _ = []
