{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

-- |
-- Module      : Web.Google.Maps.Geocoding
-- Description : Bindings to the Google Maps Geocoding API
-- Copyright   : (c) Mike Pilgrem 2017
-- Maintainer  : public@pilgrem.com
-- Stability   : experimental
-- 
-- The <https://developers.google.com/maps/documentation/geocoding/intro Google Maps Geocoding API>
-- provides a direct way to access geocoding and reverse geocoding services via
-- an HTTP request.
--
-- The 'components' and optional parameters in a geocoding request are not yet
-- implemented. The reverse geocoding request is not yet implemented.
--
-- Below is an example of use.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > module Main (main) where
-- >
-- > import Data.Text (Text)
-- > import Data.Text.IO as T (getLine, putStr)
-- > import Network.HTTP.Client (newManager)
-- > import Network.HTTP.Client.TLS (tlsManagerSettings)
-- > import Web.Google.Maps.Geocoding (Address (..), geocode, GeocodingResponse (..),
-- >     Geometry (..), Key (..), Location (..), Result (..), Status (..))
-- > import System.IO (hFlush, stdout)
-- >
-- > main :: IO ()
-- > main = do
-- >     txt <- input "Enter full address: "
-- >     mgr <- newManager tlsManagerSettings
-- >     let apiKey = Key "<GOOGLE_API_KEY>"
-- >     result <- geocode mgr apiKey (Address txt)
-- >     case result of
-- >         Right response -> do
-- >             let s = status response
-- >             case s of
-- >                OK -> print $ location $ geometry $ head $ results response
-- >                 _  -> putStrLn $ "Error! Status: " ++ show s
-- >         _ -> putStrLn $ "Error! Result:\n" ++ show result
-- >
-- > input :: Text -> IO Text
-- > input msg = T.putStr msg >> hFlush stdout >> T.getLine
module Web.Google.Maps.Geocoding
       ( -- * Functions
         geocode
         -- * API
       , GoogleMapsGeocodingAPI
       , api
         -- * Types
       , Key                  (..)
       , Address              (..)
       , GeocodingResponse    (..)
       , Status               (..)
       , Result               (..)
       , AddressType          (..)
       , AddressComponent     (..)
       , PostcodeLocality     (..)
       , Geometry             (..)
       , PlaceId              (..)
       , Location             (..)
       , LocationType         (..)
       , Viewport             (..)
       ) where

import           Data.Aeson hiding (Result)
import           Data.Aeson.Types (Options (..))
import           Data.Foldable (asum)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T (unpack)
import           GHC.Generics
import           Network.HTTP.Client (Manager)
import           Servant.API
import           Servant.Client

-- | API key
newtype Key = Key Text
    deriving (Eq, Show, ToHttpApiData)

-- | Address
newtype Address = Address Text
    deriving (Eq, Show, ToHttpApiData)

-- | Geocoding Reponse
data GeocodingResponse = GeocodingResponse
    { status        :: Status
    , error_message :: Maybe Text
    , results       :: [Result]
    } deriving (Eq, Show, Generic)

instance FromJSON GeocodingResponse

-- | Contains the status of the request and may contain debugging information to
--  help you track down why geocoding is not working.
data Status
    = OK              -- ^ Indicates that no errors occurred; the address was
                      -- successfully parsed and at least one geocode was
                      -- returned.
    | ZeroResults     -- ^ Indicates that the geocode was successful but
                      -- returned no results. This may occur if the geocoder was
                      -- passed a non-existent address.
    | OverQueryLimit
    | RequestDenied
    | InvalidRequest  -- ^ Generally indicates that the query (address,
                      -- components or latlng) is missing.
    | UnknownError
    deriving (Eq, Show)

instance FromJSON Status where
    parseJSON = withText "Status" $ \t -> case t of
        "OK"               -> return OK
        "ZERO_RESULTS"     -> return ZeroResults
        "OVER_QUERY_LIMIT" -> return OverQueryLimit
        "REQUEST_DENIED"   -> return RequestDenied
        "INVALID_REQUEST"  -> return InvalidRequest
        "UNKNOWN_ERROR"    -> return UnknownError
        _                  -> fail $ "Unrecognised status type, namely: " ++
                                  T.unpack t

-- | A result of the geocoder.
data Result = Result
    { types :: [AddressType]
    , formatted_address :: Text
    , address_components :: [AddressComponent]
    , postcode_localities :: Maybe [PostcodeLocality]
    , geometry :: Geometry
    , partial_match :: Maybe Bool
    , place_id :: PlaceId
    } deriving (Eq, Show, Generic)

instance FromJSON Result

-- | Address (and address component) type: The list of types provided by Google
-- (as at 4 March 2017) is incomplete.
data AddressType = AddressType Text
    deriving (Eq, Show, Generic)

instance FromJSON AddressType

-- | Address component
data AddressComponent = AddressComponent
    { address_component_types      :: [AddressType]
    , long_name  :: Text
    , short_name :: Text
    } deriving (Eq, Show, Generic)

instance FromJSON AddressComponent where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = \l -> case l of
            "address_component_types" -> "types"
            _ -> l
        }

-- | Postcode locality: a locality contained in a postal code
newtype PostcodeLocality = PostcodeLocality Text
    deriving (Eq, Show, Generic)

instance FromJSON PostcodeLocality

-- | Geometry
data Geometry = Geometry
    { location :: Location
    , location_type :: LocationType
    , viewport :: Viewport
    , bounds :: Maybe Viewport
    } deriving (Eq, Show, Generic)

instance FromJSON Geometry

-- | Location
data Location = Location
    { lat :: Double
    , lng :: Double
    } deriving (Eq, Show, Generic)

instance FromJSON Location

-- | Location type
data LocationType
    = Rooftop
    | RangeInterpolated
    | GeometricCenter
    | Approximate
    deriving (Eq, Show)

instance FromJSON LocationType where
    parseJSON = withText "LocationType" $ \t -> case t of
        "ROOFTOP"            -> return Rooftop
        "RANGE_INTERPOLATED" -> return RangeInterpolated
        "GEOMETRIC_CENTER"   -> return GeometricCenter
        "APPROXIMATE"        -> return Approximate
        _ -> fail $ "Unrecognised location type, namely: " ++ T.unpack t

-- | Viewport
data Viewport = Viewport
    { southwest :: Location
    , northeast :: Location
    } deriving (Eq, Show, Generic)

instance FromJSON Viewport

-- | Place id
newtype PlaceId = PlaceId Text
    deriving (Eq, Show, Generic)

instance FromJSON PlaceId

-- | Google Translate API
type GoogleMapsGeocodingAPI
    =  "json"
    :> QueryParam "key" Key
    :> QueryParam "address" Address
    :> Get '[JSON] GeocodingResponse

-- | API type
api :: Proxy GoogleMapsGeocodingAPI
api = Proxy

geocode'
    :: Maybe Key
    -> Maybe Address
    -> ClientM GeocodingResponse
geocode' = client api

googleApis :: BaseUrl
googleApis = BaseUrl Https "maps.googleapis.com" 443 "/maps/api/geocode"

-- | Geocode
geocode
    :: Manager
    -> Key
    -> Address
    -> IO (Either ServantError GeocodingResponse)
geocode mgr key address =
    runClientM (geocode' (Just key) (Just address)) (ClientEnv mgr googleApis)
