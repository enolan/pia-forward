module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Default
import Data.Functor
import Data.List.Extra
import Data.Maybe
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.Info
import System.Process

main = do
    clientId <- trim <$> readFile "/home/enolan/.pia_client_id"
    man <- newManager tlsManagerSettings
    forever $ do
        res <- try $ do
            vpnIP <- ipv4 . fromJust . find ((== "tun0") . name) <$> getNetworkInterfaces
            let params = [
                    ("user", Just "XXX"),
                    ("pass", Just "XXX"),
                    ("client_id", Just (B.pack clientId)),
                    ("local_ip", Just (B.pack $ show vpnIP))]
                body = B.drop 1 $ queryString $ setQueryString params def
            req <- parseUrl "https://www.privateinternetaccess.com/vpninfo/port_forward_assignment"
            let req' = req {method = "POST", requestBody = RequestBodyBS body}
            print $ case requestBody req' of
                RequestBodyBS bs -> bs
            resp <- withResponse req' man (brConsume . responseBody)
            B.putStrLn $ B.concat resp
            createProcess $ shell "date"
        case res of
            Left (ex :: SomeException) -> print ex
            Right _ -> return ()
        threadDelay $ 30*60*1000000
