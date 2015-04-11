module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (replicateM, forever)
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Default (Default(..))
import Data.Functor ((<$>))
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Monoid (mconcat)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Format (left)
import Data.Text.Format.Types (Hex(..))
import Data.Text.IO (getLine)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Info
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserConfigFile)
import System.FilePath (takeDirectory)
import System.Process (shell, createProcess)
import System.Random (Random(..))

main :: IO ()
main = do
    man <- newManager tlsManagerSettings
    cfg <- try readConfig
    cfg' <- case cfg of
        Left (ex :: SomeException) -> do
            putStrLn ("Couldn't read config: " ++ show ex)
            setupNewConfig
        Right cfg'' -> return cfg''
    case cfg' of
        Config {clientId, user, pass} -> forever $ do
            res <- try $ do
                vpnIP <- ipv4 . fromJust . find ((== "tun0") . name) <$> getNetworkInterfaces
                let params = [
                        ("user", Just $ encodeUtf8 user),
                        ("pass", Just $ encodeUtf8 pass),
                        ("client_id", Just $ encodeUtf8 clientId),
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

readConfig :: IO Config
readConfig = (fromJust . decode) <$> (getConfigPath >>= LB.readFile)
