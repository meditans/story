{-# LANGUAGE OverloadedStrings #-}

import QuickTime
import Data.Function
import Parser
import Data.List

import qualified Data.ByteString as B
-- Lo scopo di questo file e' eseguire il dialogo di prova:

main :: IO ()
main = do
  dia <- dialogoProva
  eseguiDialogo dia
  return ()

eseguiDialogo dia = do
  -- trovo la battuta iniziale
  let st = minimumBy (compare `on` bIndex) (dBattute dia)
  res <- eseguiBattuta st
  maybe (B.putStrLn "Non sai cosa dire, eh? Allora muori!!!" >> return Nothing)
        (eseguiBattuta . selezionaBattuta dia)
        res
  return ()


eseguiBattuta :: Battuta -> IO (Maybe Int)
eseguiBattuta bat = do
  B.putStrLn . bContent $ bat
  ww <- timed 4 $ dialogues (bRisposte bat)
  return ww
