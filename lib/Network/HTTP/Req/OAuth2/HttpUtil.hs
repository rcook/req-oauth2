{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Req.OAuth2.HttpUtil
    ( acceptLanguage
    , hasResponseStatus
    ) where

import qualified Network.HTTP.Client as HTTP (HttpException(..), HttpExceptionContent(..), responseStatus)
import           Network.HTTP.Req (HttpException(..), Option, header)
import           Network.HTTP.Types (Status)

hasResponseStatus :: HttpException -> Status -> Bool
hasResponseStatus
    (VanillaHttpException (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException response _))) status =
    HTTP.responseStatus response == status
hasResponseStatus _ _ = False

acceptLanguage :: Option scheme
acceptLanguage = header "Accept-Language" "en_US"
