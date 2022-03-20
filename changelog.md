# 0.7.0.1

* Update dependencies for GHC 9.0.2 and revisit upper bounds

# 0.7.0.0

* Add additional languages

* Update dependencies for GHC 8.10.4 and revisit upper bounds

# 0.5.0.1

* Update dependencies for GHC 8.8.1

# 0.5.0.0

* Update documentation to reflect changes to Google Maps Platform

* Rename module `Web.Google.Maps.Geocoding` as `Web.Google.Geocoding` and
  `GoogleMapsGeocodingAPI` as `GoogleGeocodingAPI`

* Update dependencies for GHC 8.6.2 and revisit upper bounds

# 0.4.0.2

* Update dependencies for GHC 8.6.1

# 0.4.0.1

* Update dependencies for GHC 8.4.3

# 0.4.0.0

* Depend on more recent version of `google-static-maps` and `servant` packages
  and, consequently, `servant-client` and `aeson` packages

# 0.3.0.0

* Implement reverse (back) geocoding (`backGeocode`)

* Implement `components`, `bounds`, `language` and `region` optional parameters

* Change latitude/longitude type to `LatLng` from `Location`

# 0.2.0.0

* Move `geocode` from the end of the base URL to the start of the API type

* Depend on package `google-static-maps` for common types `Key` and `Location`
  and function `googleMapsApis`

# 0.1.0.1

* Changes to documentation only

# 0.1.0.0

* Launch implementation. The `components` and optional parameters in a geocoding
  request are not yet implemented. The reverse geocoding request is not yet
  implemented
