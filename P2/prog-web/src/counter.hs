{-# LANGUAGE OverloadedStrings #-}

module Main
where
import System.IO
import Control.Exception

-- ****************************************************************

main :: IO ()
main = do
    -- Llegeix el valor del fitxer comptador i l'incrementa
    -- Escriu el nou valor al fitxer comptador
    -- Treu la sortida adequada (amb el nou valor)
    num <- (fmap (+1) readCounter)
    writeCounter num
    putStrLn "Content-Type: TIPUS_MIME" 
    putStrLn ""
    putStrLn (show num)

readCounter :: IO Int
readCounter = do
    r <- try $ do
        h <- openFile counterFilePath ReadMode
        content <- hGetLine h
        hClose h
        return $ read content
    case (r :: Either SomeException Int) of
        Right i -> return i
        Left exc -> do
            writeCounter 0
            return 0

writeCounter :: Int -> IO ()
writeCounter i = do
    h <- openFile counterFilePath WriteMode
    hPutStrLn h $ show i
    hClose h

counterFilePath = "counter.data"

