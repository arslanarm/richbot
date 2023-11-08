{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module VendingMachineApi where
import Data.Text (Text)
import Data.Aeson
    ( (.:), withObject, FromJSON(parseJSON), Value(Object) )
import Network.HTTP.Simple ( parseRequest, httpJSON, getResponseBody, setRequestHeader )
import Data.Int ( Int64 )
import Control.Applicative ( Alternative(empty) )
import Data.ByteString (ByteString)
import Network.HTTP.Client (Request, urlEncodedBody)
import Network.HTTP.Client.MultipartFormData (formDataBody, partBS)
import Data.ByteString.Char8 (pack)

commonHeaders :: Request -> Request
commonHeaders = setRequestHeader "x-api-lang" ["en"]
                . setRequestHeader "x-api-version" ["1.4"]
                . setRequestHeader "x-app-version" ["24U/Android-30/MI 9/2.2.28"]
                

getItems :: (Request -> Request) -> [Char] -> [Char] -> IO VendingMachineData
getItems authorize machine address = do
    request <-  parseRequest ("GET " <> machineUrl machine address)
    response <- httpJSON ((authorize . commonHeaders) request)
    let body = getResponseBody response :: VendingMachineData
    return body
    where
        machineUrl id address = address ++ "/api/Machine/" ++ id ++ "@uonline"

authorize :: ByteString -> Request -> Request
authorize token = setRequestHeader "Authorization" ["Bearer " <> token]

authorizeToken :: Token -> Request -> Request
authorizeToken Token{token = token} = authorize (pack token)

getToken :: ByteString -> ByteString -> ByteString -> String -> IO Token
getToken username password devid address = do
    request <- parseRequest ("POST " <> address <> "/Token")
    let form = [("grant_type", "password"), ("username", username), ("password", password), ("devid", devid)]
    let request' = urlEncodedBody form request
    response <- httpJSON (commonHeaders request')
    let body = getResponseBody response :: Token
    return body

data VendingMachineData = VendingMachineData
    { machine :: VendingMachineInfo
    , statusCode :: Bool
    , errorCode :: Int
    } deriving (Show)

instance FromJSON VendingMachineData where
    parseJSON (Object v) = VendingMachineData <$>
                           v .: "machine" <*>
                           v .: "status" <*>
                           v .: "errorCode"
    parseJSON _ = empty


data VendingMachineInfo = VendingMachineInfo
    { id :: Int64
    , status :: Text
    , planogram :: [VendingMachineItem]
    , decimalPoint :: Int64
    } deriving (Show)

instance FromJSON VendingMachineInfo where
    parseJSON (Object v) = VendingMachineInfo <$>
                           v .: "id" <*>
                           v .: "status" <*>
                           v .: "planogram" <*>
                           v .: "decimalPoint"
    parseJSON _ = empty

data VendingMachineItem = VendingMachineItem
    { selectionId :: Int64
    , position :: Text
    , name :: Text
    , itemType :: Int64
    , price :: Double
    , discount :: Double
    , itemAmount :: Int
    , width :: Double
    , imageId :: Text
    , imageVersion :: Int64
    } deriving (Show)

instance FromJSON VendingMachineItem where
    parseJSON (Object v) = VendingMachineItem <$>
                           v .: "selectionId" <*>
                           v .: "position" <*>
                           v .: "name" <*>
                           v .: "type" <*>
                           v .: "price" <*>
                           v .: "discount" <*>
                           v .: "amount" <*>
                           v .: "width" <*>
                           v .: "imageId" <*>
                           v .: "imageVersion"
    parseJSON _ = empty

data Token = Token
    { token        :: String
    , refreshToken :: String
    , expiresIn    :: Int64
    , username     :: String
    } deriving (Show)

instance FromJSON Token where
    parseJSON = withObject "Token" $ \v -> Token
        <$> v .: "access_token"
        <*> v .: "refresh_token"
        <*> v .: "expires_in"
        <*> v .: "userName"