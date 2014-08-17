module QuickTime where

import qualified Data.ByteString as S
import           Data.Monoid
import           Parser
import           System.Timeout

data Interaction = Ok | No deriving (Show, Eq)

quickTime :: String -> Int -> IO Interaction
quickTime str t = do
  putStrLn $ "Digita " ++ str ++ " in " ++ show t ++ " secondi"
  resp <- getLine
  if resp == str
     then return Ok
     else quickTime str t

timed :: Int -> IO a -> IO (Maybe a)
timed t act = timeout (t * 1000000) act



dialogues :: [Risposta] -> IO Int
dialogues ss = do
    mapM_ (S.putStrLn . prettyPrint) (ss)
    n <- read `fmap` getLine
    if n >= 1 && n <= length ss
       then return $ n-1
       else dialogues ss 
  where numbers = map (\n -> show n ++ ". ") [1..]

