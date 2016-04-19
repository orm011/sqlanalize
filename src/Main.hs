import System.Environment
import Language.SQL.SimpleSQL.Parser
import Language.SQL.SimpleSQL.Syntax
import Data.List
import Data.Maybe
import Data.Either

main :: IO ()
main = do a <- getArgs
          case a of
            [str] -> do f <- readFile str
                        let parsed  = partitionEithers $ map (parseQueryExpr str Nothing)  $ lines f
                            num_errors = length $ fst $ parsed
                            good_ones = snd $ parsed
                            idens = map get_all_idens good_ones
                            in sequence_ $ (map (putStrLn . show) idens) ++ [putStrLn $ show num_errors]
            _ -> error "we need an input file name"

{- finds the identifiers used within an expression, possibly deep within -}
get_idens :: ValueExpr -> [String]
get_idens (Iden ls) = [intercalate "." (map show ls)]
get_idens Star = ["star"] {- fix later by using schema -}
get_idens (App _ vals) = concat $ map get_idens vals
get_idens (AggregateApp { aggArgs, aggFilter }) = concat $ (map get_idens aggArgs) ++ (map get_idens (maybeToList aggFilter))
get_idens _ = []

get_all_idens :: QueryExpr -> [String]
get_all_idens Select { qeSelectList } = concat $ map (get_idens . fst) qeSelectList
get_all_idens  _ = []


