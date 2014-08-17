{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative ((*>), (<*), (<*>), (<$>))
import qualified Data.ByteString as B

import Data.ByteString.Char8 (ByteString, pack)
import Text.Parsec
import Text.Parsec.ByteString

data Risposta = Risposta
    { index :: Int
    , to :: Int
    , content :: ByteString
    } deriving (Show, Eq)

risposta :: Parser Risposta
risposta = do
  _ <- string "#RISPOSTA"           <* space
  i <- string "#INDEX"    *> braces <* space
  t <- string "#TO"       *> braces <* space
  c <- string "#CONTENT"  *> braces 
  return $ Risposta (read i) (read t) (pack c)

data Battuta = Battuta
    { bIndex :: Int
    , bContent :: ByteString
    , bRisposte :: [Risposta]
    } deriving (Show, Eq)

battuta :: Parser Battuta
battuta = do
  _ <- string "#BATTUTA" <* space
  i <- string "#INDEX" *> braces <* space
  c <- string "#CONTENT" *> braces <* newline
  r <- endBy risposta newline
  return $ Battuta (read i) (pack c) r

data Dialogo = Dialogo
    { dIndex :: Int
    , dBattute :: [Battuta]
    } deriving (Show, Eq)

dialogo :: Parser Dialogo
dialogo = do
  _ <- string "#DIALOGO" <* space
  i <- string "#INDEX" *> braces <* newline
  b <- endBy (try battuta) newline
  return $ Dialogo (read i) b

braces :: Parser String
braces = between (string "{") (string "}") (many $ noneOf "}")

a :: IO ByteString
a = B.readFile "simpleDialog.txt"

main = do
  aa <- a
  print $ parse dialogo "argh" aa
