import Text.ParserCombinators.Parsec
import Numeric (readOct,readHex)
import System.Environment
import Control.Monad.Except
import Data.List (minimumBy)
import Data.Maybe (mapMaybe)

type Node = Int
type Graph = [(Node,[(Node,Int)])]

cmpBy :: Ord b => (a -> b) -> a -> a -> Ordering
cmpBy foo a b = foo a `compare` foo b

shortestNeighbour :: [(Node,Int)] -> Node -> Maybe (Node,Int)
shortestNeighbour [(node,cost)] dest = if node == dest then Just (node,cost) else Nothing
shortestNeighbour [] _ = Nothing
shortestNeighbour list dest = case mapMaybe (\x -> shortestNeighbour [x] dest) list of
	[] -> Nothing
	list -> Just $ minimumBy (\a b -> cmpBy (\(_,cost) -> cost) a b) list

shortestPath :: Node -> Node -> Graph -> Maybe Int
shortestPath start dest nodeMap = if start == dest then Just 0 else do
	neighbours <- lookup start nodeMap
	case shortestNeighbour neighbours dest of
		Just (_,cost) -> Just cost
		Nothing ->
			case mapMaybe (\(node,cost) ->
				case shortestPath node dest $ filter (\(i,_) -> i /= start) nodeMap of
					Just i -> Just (cost + i)
					Nothing -> Nothing
			) neighbours of
				[] -> Nothing
				list -> Just $ minimum list

parseArgs :: [String] -> IO (Int,Int)
parseArgs [startstr,deststr] = return (read startstr,read deststr)

parseGraph :: Parser Graph
parseGraph = between (char '[') (char ']') (sepBy parseNode $ char ',')

parseNode :: Parser (Node,[(Node,Int)])
parseNode = do
	char '('
	node <- parseNumber
	char ','
	neighbours <- parseNeighbours
	char ')'
	return (node,neighbours)
	
parseNeighbours :: Parser [(Node,Int)]
parseNeighbours = between (char '[') (char ']') (sepBy parseNeighbour $ char ',')

parseNeighbour :: Parser (Node,Int)
parseNeighbour = do
	char '('
	node <- parseNumber
	char ','
	cost <- parseNumber
	char ')'
	return (node,cost)

parseNumber :: Parser Int
parseNumber = do
	str <- many digit
	return $ read str

main :: IO ()
main = do
	args <- getArgs
	pos <- parseArgs args
	input <- getContents
	case parse parseGraph "graph" input of
		Left err -> print err
		Right graph -> maybe (print "no path") (print) $ let (start,dest) = pos in
			shortestPath start dest graph

