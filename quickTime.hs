module QuickTime where

import System.Timeout


data Interaction = Ok | No deriving (Show, Eq)

quickTime :: String -> Int -> IO Interaction
quickTime str t = do
  putStrLn $ "Digita " ++ str ++ " in " ++ show t ++ " secondi"
  resp <- getLine
  if resp == str
     then return Ok
     else quickTime str t

quickTime' :: String -> Int -> IO (Maybe Interaction)
quickTime' str t = timeout (t * 1000000) (quickTime str t)

timed :: Int -> IO a -> IO (Maybe a)
timed t act = timeout (t * 1000000) act


dialogues :: [String] -> IO String
dialogues ss = do
    mapM_ print $ zipWith (++) numbers ss
    n <- read `fmap` getLine
    if n >= 1 && n <= length ss
       then return $ ss !! (n-1)
       else dialogues ss 
  where numbers = map (\n -> show n ++ ". ") [1..]

main :: IO ()
main = do
  quickTime "ciao" 4
  return ()
