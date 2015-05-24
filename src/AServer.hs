module Main where

import Data.Maybe (listToMaybe)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)

import Network.Simple.TCP (serve, HostPreference (..), Socket, SockAddr)
import Network.Socket (socketToHandle)
import System.IO
import Control.Exception (try, finally, displayException, IOException)
import System.Directory (doesFileExist)

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TQueue
import Data.IVar.Simple as IVar

import System.Hardware.Arduino
import Data.Word (Word8)

-- | Try different paths. When connection is lost to arduino, it can change path to next available
arduinoPaths :: [String]
arduinoPaths = ["/dev/ttyACM0", "/dev/ttyACM1"]

port :: String
port = "8020"

debug :: Bool
debug = False

arduinoConfig :: Arduino ()
arduinoConfig = do
    -- Relay config:
    forM_ (13 : [2 .. 10]) $ \ n ->
        setPinMode (digital n) OUTPUT
    forM_ ([3 .. 10] ++ [2, 13]) $ \ n ->
        digitalWrite (digital n) True
    setPinMode (analog 1) ANALOG

type FutureRes a = IVar.IVar a

waitForResult :: FutureRes a -> IO a
waitForResult  = return . IVar.read

type PinId = Word8
data ParsedCommand = DW PinId Bool -- ^ DigitalWrite PinId
                   | DR PinId      -- ^ DigitalRead
                   | AR PinId      -- ^ AnalogRead
                   deriving (Show, Read)

data Command = DigitalWrite Pin Bool
             | DigitalRead Pin (FutureRes Bool)
             | AnalogRead Pin (FutureRes Double)

type CommandQueue = TQueue Command

putCommand :: CommandQueue -> Command -> IO ()
putCommand q c = atomically $ writeTQueue q c

handleConn :: CommandQueue -> (Socket, SockAddr) -> IO ()
handleConn q (connectionSocket, remoteAddr) = do
    threadId <- forkIO $ do
        handle <- socketToHandle connectionSocket ReadWriteMode
        receiveCommands q handle `finally` hClose handle
    putStrLn $ "[INFO] Forked (thread:" ++ show threadId ++ ") Connection from " ++ show remoteAddr
    return ()

receiveCommands :: CommandQueue -> Handle -> IO ()
receiveCommands q handle = do
    hSetBuffering handle LineBuffering
    loop
  where
    loop = do
        line <- hGetCmd handle "" -- hGetLine handle -- openhab was not able to send a newline :(
        case parseCommand line of
            Just cmd -> do
                result <- handleCommand cmd q
                hPutStrLn handle result
            Nothing ->
                putStrLn $ "[WARN] parsing failed: " ++ line
        loop

    -- XXX: openhab couldn't send newline, implement our own parser
    hGetCmd :: Handle -> String -> IO String
    hGetCmd h partial = do
        chr <- hGetChar h


        case chr of
            ';'  -> return partial
            '\n' -> return partial
            '\r' -> hGetCmd handle partial
            _    -> hGetCmd handle (partial ++ [chr])



parseCommand :: String -> Maybe ParsedCommand
parseCommand = fmap fst . listToMaybe . reads

handleCommand :: ParsedCommand -> CommandQueue -> IO String
handleCommand (DW pinId bool) q = do
    putCommand q $ DigitalWrite (digital pinId) bool
    return ""
handleCommand (DR pinId) q = do
    future <- IVar.new
    putCommand q $ DigitalRead (digital pinId) future
    return . show $ IVar.read future
handleCommand (AR pinId) q = do
    future <- IVar.new
    putCommand q $ AnalogRead (analog pinId) future
    return . show $ IVar.read future

execCommand :: Command -> Arduino ()
execCommand (DigitalWrite pinId bool) = digitalWrite pinId bool
    -- setPinMode pinId OUTPUT
execCommand (DigitalRead pinId future) = do
    result <- digitalRead pinId
    liftIO $ IVar.write future result
execCommand (AnalogRead pinId future) = do
    result <- analogRead pinId
    let normalizedRes = fromIntegral result / 1023
    liftIO $ IVar.write future normalizedRes


arduinoController :: CommandQueue -> IO ()
arduinoController queue =
    connectLoop $ cycle arduinoPaths

  where
    connectLoop paths = do
        exists <- doesFileExist (head paths)
        if exists then do
            result <- try $ connectArduino (head paths)
            case result of
                (Right _) ->
                    putStrLn "[INFO] Arduino loop ended normally."
                (Left e) -> do
                    putStrLn $
                        "[WARN] Arduino connection lost: " ++ displayException (e :: IOException)
                    threadDelay $ 10 * 1000000
                    connectLoop (tail paths)
        else connectLoop (tail paths)

    connectArduino path =
        withArduino debug path $ do
            arduinoConfig
            loop
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
    serve (Host "127.0.0.1") port $ handleConn q



