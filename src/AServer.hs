module Main where

import Data.Maybe (listToMaybe)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)

import Network.Simple.TCP (serve, HostPreference(..), Socket, SockAddr)
import Network.Socket (socketToHandle)
import System.IO
import Control.Exception (finally)

import Control.Concurrent (forkIO)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TQueue
import Data.IVar.Simple as IVar

import System.Hardware.Arduino
import Data.Word (Word8)

arduinoPath :: String
arduinoPath = "/dev/ttyACM1"

port :: String
port = "8020"

arduinoConfig :: Arduino ()
arduinoConfig = do
    -- Relay config:
    forM_ (13:[2..10]) $ \n ->
        setPinMode (digital n) OUTPUT
    forM_ [3..10] $ \n ->
        digitalWrite (digital n) True

type FutureRes a = IVar.IVar a

waitForResult :: FutureRes a -> IO a
waitForResult  = return . IVar.read

type PinId = Word8
data ParsedCommand = DW PinId Bool -- ^ WriteDigital PinId
                   | DR PinId      -- ^ ReadDigital
                   | AR PinId      -- ^ ReadAnalog
                   deriving (Show, Read)

data Command = WriteDigital Pin Bool
             | ReadDigital (FutureRes Bool)
             | ReadAnalog (FutureRes Double)

type CommandQueue = TQueue Command

putCommand :: CommandQueue -> Command -> IO ()
putCommand q c = atomically $ writeTQueue q c

handleConn :: CommandQueue -> (Socket, SockAddr) -> IO ()
handleConn q (connectionSocket, remoteAddr) = do
    putStrLn $ "[INFO] Connection from " ++ show remoteAddr
    handle <- socketToHandle connectionSocket ReadWriteMode
    receiveCommands q handle `finally` hClose handle

receiveCommands :: CommandQueue -> Handle -> IO ()
receiveCommands q handle = do
    hSetBuffering handle LineBuffering
    loop
  where
    loop = do
        line <- hGetLine handle
        case parseCommand line of
            Just cmd -> do 
                result <- handleCommand cmd q
                hPutStrLn handle result
            Nothing ->
                putStrLn $ "[WARN] parsing failed: " ++ line
        loop

parseCommand :: String -> Maybe ParsedCommand
parseCommand = fmap fst . listToMaybe . reads

handleCommand :: ParsedCommand -> CommandQueue -> IO String
handleCommand (DW pinId bool) q = do
    putCommand q $ WriteDigital (digital pinId) bool
    return ""

execCommand :: Command -> Arduino ()
execCommand (WriteDigital pinId bool) = do
    --setPinMode pinId OUTPUT
    digitalWrite pinId bool

arduinoController :: CommandQueue -> IO ()
arduinoController queue =
    withArduino True arduinoPath $ do
        arduinoConfig
        loop
  where
    loop :: Arduino ()
    loop = do
        cmd <- liftIO . atomically $ readTQueue queue
        execCommand cmd
        loop
    


main :: IO ()
main = do
    q <- newTQueueIO
    putStrLn "[INFO] Starting arduino handler"
    _ <- forkIO $ arduinoController q
    putStrLn $ "[INFO] Starting arduinoserv at localhost port " ++ port
    serve (Host "127.0.0.1") port (handleConn q)



