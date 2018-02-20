module Main (main) where
import Csv.Parser ( parseFile )

main = do 
  putStrLn "CSV Parser"

  result <- parseFile "test.csv" 
  mapM_ ((mapM_ (putStr . formatEntry)) . formatLine) result
    where formatLine x = x ++ [['\n']]
          formatEntry x = x ++ [','] 
    

