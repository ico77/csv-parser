module Csv.Parser (parseLine, parseFile) where

data State = Begin | NonQuoted | Quoted | EndNonQuoted | EndQuoted

instance Show State where
    show NonQuoted   = "NonQuoted"
    show EndNonQuoted = "EndNonQuoted"
    show Begin = "Begin"

type Event = Char
type Action = [String] -> [String]

transition :: State -> Event -> (State, Maybe Action)
transition Begin ','          = (EndNonQuoted, Just (addElement ""))
transition Begin '"'          = (Quoted, Nothing)
transition Begin event        = (NonQuoted, Just (addElement [event]))
transition NonQuoted ','      = (EndNonQuoted, Nothing)
transition NonQuoted event    = (NonQuoted, Just (appendToLastElement [event]))
transition EndNonQuoted ','   = (EndNonQuoted, Just (addElement ""))
transition EndNonQuoted event = (NonQuoted, Just (addElement [event]))

addElement :: String -> [String] -> [String]
addElement elem xs = xs ++ [elem]

appendToLastElement :: String -> [String] -> [String]
appendToLastElement content xs = do
                                   let lastToken = last xs
                                   (init xs) ++ [lastToken ++ content]

parse :: String -> [String] -> State -> [String]
parse (event:xs) ys state = case event of '\r' -> parse xs ys state
                                          _ -> parse xs ys' newState
                                            where (newState, action) = transition state event 
                                                  ys' = case action of
                                                    Nothing -> ys
                                                    Just f -> f ys
parse [] ys state = ys

parseLine :: String -> [String]
parseLine [] = []
parseLine xs = parse xs [] Begin

parseFile :: FilePath -> IO [[String]] 
parseFile path = do
                   csvContent <- readFile path
                   let csvLines = lines csvContent
                   return $ fmap (parseLine) csvLines
