
{-# LANGUAGE OverloadedStrings #-}

module Main
where
import qualified Network.Wai.Handler.CGI as W (run)
import qualified Control.Exception as E

import           Handler
import           CalcApp

-- ****************************************************************

-- Inici d'execució del CGI.
-- Usa les funcions:
--      Adaptador WAI-CGI:           run :: Application -> IO ()
--      Executor del monad Handler:  dispatchHandler :: Handler HandlerResponse -> Application
--      Aplicació calculadora:       calcApp :: Handler HandlerResponse
main :: IO ()
main = do
    -- try :: Exception e => IO a -> IO (Either e a)
    r <- E.try $
        W.run $ dispatchHandler calcApp
    case r of
        Right _ -> pure ()
        Left exc -> do
            -- Exception on initialization
            putStrLn "Status: 500 Internal Server Error"
            putStrLn "Content-Type: text/plain"
            putStrLn ""
            putStrLn "Exception on initialization (while excution of 'calcApp'): "
            putStrLn $ "    " ++ show (exc :: E.SomeException)

