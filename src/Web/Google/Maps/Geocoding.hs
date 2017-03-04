{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

{-|
Module      : Web.Google.Maps.Geocoding
Description : Bindings to the Google Maps Geocoding API
Copyright   : (c) Mike Pilgrem 2017
Maintainer  : public@pilgrem.com
Stability   : experimental

The <https://developers.google.com/maps/documentation/geocoding/intro Google Maps Geocoding API>
provides a direct way to access geocoding and reverse geocoding services via an
HTTP request.
-}
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
       , AddressComponentType (..)
       , Location             (..)
       , LocationType         (..)
       , Viewport             (..)
       ) where

import           Data.Aeson hiding (Result)
import           Data.Aeson.Types (Options (..))
import           Data.Foldable (asum)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
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
        _                  -> fail "Unrecognised status type"

-- | A result of the geocoder.
data Result = Result
    { types :: [AddressType]
    , formatted_address :: Text
    , address_components :: [AddressComponent]
    , postcode_localities :: Maybe [PostcodeLocality]
    , geometry :: Geometry
    , partial_match :: Bool
    , place_id :: PlaceId
    } deriving (Eq, Show, Generic)

instance FromJSON Result

-- | Address type
data AddressType
  = StreetAddress
  | Route
  | Intersection
  | Political
  | Country
  | AdministrativeAreaLevel1
  | AdministrativeAreaLevel2
  | AdministrativeAreaLevel3
  | AdministrativeAreaLevel4
  | AdministrativeAreaLevel5
  | ColloquialArea
  | Locality
  | Ward
  | Sublocality
  | SublocalityLevel1
  | SublocalityLevel2
  | SublocalityLevel3
  | SublocalityLevel4
  | SublocalityLevel5
  | Neighborhood
  | Premise
  | Subpremise
  | PostalCode
  | NaturalFeature
  | Airport
  | Park
  | PointOfInterest
  deriving (Eq, Show)

instance FromJSON AddressType where
    parseJSON = withText "AddressType" $ \t -> case t of
        "street_address" -> return StreetAddress
        "route" -> return Route
        "intersection" -> return Intersection
        "political" -> return Political
        "country" -> return Country
        "administrative_area_level_1" -> return AdministrativeAreaLevel1
        "administrative_area_level_2" -> return AdministrativeAreaLevel2
        "administrative_area_level_3" -> return AdministrativeAreaLevel3
        "administrative_area_level_4" -> return AdministrativeAreaLevel4
        "administrative_area_level_5" -> return AdministrativeAreaLevel5
        "colloquial_area" -> return ColloquialArea
        "locality" -> return Locality
        "ward" -> return Ward
        "sublocality" -> return Sublocality
        "sublocality_level_1" -> return SublocalityLevel1
        "sublocality_level_2" -> return SublocalityLevel2
        "sublocality_level_3" -> return SublocalityLevel3
        "sublocality_level_4" -> return SublocalityLevel4
        "sublocality_level_5" -> return SublocalityLevel5
        "neighborhood" -> return Neighborhood
        "premise" -> return Premise
        "subpremise" -> return Subpremise
        "postal_code" -> return PostalCode
        "natural_feature" -> return NaturalFeature
        "airport" -> return Airport
        "park" -> return Park
        "point_of_interest" -> return PointOfInterest
        _ -> fail "Unrecognised address type"

-- | Address component
data AddressComponent = AddressComponent
    { address_component_types      :: [AddressComponentType]
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
        _ -> fail "Unrecognised location type"

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

-- | Address component
data AddressComponentType
    = AddressType AddressType
    | Floor
    | Establishment
    | Parking
    | PostBox
    | PostalTown
    | Room
    | StreetNumber
    | BusStation
    | TrainStation
    | TransitStation
    deriving (Eq, Show)

instance FromJSON AddressComponentType where
    parseJSON = withText "AddressComponentType" $ \t -> asum
        [ AddressType <$> parseJSON (String t)
        , case t of
            "floor"           -> return Floor
            "establishment"   -> return Establishment
            "parking"         -> return Parking
            "post_box"        -> return PostBox
            "postal_town"     -> return PostalTown
            "room"            -> return Room
            "street_number"   -> return StreetNumber
            "bus_station"     -> return BusStation
            "train_station"   -> return TrainStation
            "transit_station" -> return TransitStation
            _ -> fail "Unrecognised address component type"
        ]

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
