module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (replicateM, forever)
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Default (Default(..))
import Data.List (find)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Format (left)
import Data.Text.Format.Types (Hex(..))
import Data.Text.IO (getLine)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Time.LocalTime (getZonedTime)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Info
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserConfigFile)
import System.FilePath (takeDirectory)
import System.Random (Random(..))

main :: IO ()
main = do
    man <- newManager tlsManagerSettings
    cfg <- try readConfig
    cfg' <- case cfg of
        Left (ex :: IOException) -> do
            putStrLn ("Couldn't read config: " ++ show ex)
            setupNewConfig
        Right Nothing -> do
            putStrLn "Couldn't parse config."
            setupNewConfig
        Right (Just cfg'') -> return cfg''
    case cfg' of
        Config {clientId, user, pass} -> forever $ do
            let exHandlers = [
                    Handler (\(ex :: HttpException) ->
                        putStrLn $ "HTTP call failed: " ++ show ex),
                    Handler (\(_  :: PIAForwardException) ->
                        putStrLn "Couldn't get VPN IP address.")]
            flip catches exHandlers $ do
                mbVpnIP <- find ((== "tun0") . name) <$> getNetworkInterfaces
                vpnIP <- case mbVpnIP of
                    Nothing -> throwIO CouldntGetIP
                    Just vpnIP' -> return $ ipv4 vpnIP'
                let params = [
                        ("user", Just $ encodeUtf8 user),
                        ("pass", Just $ encodeUtf8 pass),
                        ("client_id", Just $ encodeUtf8 clientId),
                        ("local_ip", Just (B.pack $ show vpnIP))]
                    body = B.drop 1 $ queryString $ setQueryString params def
                let req = "https://www.privateinternetaccess.com/vpninfo/port_forward_assignment"
                        {method = "POST", requestBody = RequestBodyBS body}
                time <- getZonedTime
                putStrLn $ "sending " ++ show body ++ " at " ++ show time
                resp <- httpLbs req man
                LB.putStrLn $ responseBody resp
            threadDelay $ 30*60*1000000

data PIAForwardException = CouldntGetIP deriving (Show, Typeable)
instance Exception PIAForwardException

setupNewConfig :: IO Config
setupNewConfig = do
    bytes :: [Word8] <- replicateM 16 randomIO
    let clientId =
            toStrict $ toLazyText $ mconcat $ map (left 2 '0' . Hex) bytes
    putStrLn "Enter PIA username:"
    user <- Data.Text.IO.getLine
    putStrLn "Enter PIA password:"
    pass <- Data.Text.IO.getLine
    let cfg = Config {clientId, user, pass}
    writeConfig cfg
    return cfg

data Config =
    Config {clientId :: Text, user :: Text, pass :: Text}
    deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config

getConfigPath :: IO FilePath
getConfigPath = getUserConfigFile "pia-forward" "config"

writeConfig :: Config -> IO ()
writeConfig cfg = do
    configPath <- getConfigPath
    createDirectoryIfMissing True $ takeDirectory configPath
    LB.writeFile configPath (encode cfg)

readConfig :: IO (Maybe Config)
readConfig = decode <$> (getConfigPath >>= LB.readFile)
