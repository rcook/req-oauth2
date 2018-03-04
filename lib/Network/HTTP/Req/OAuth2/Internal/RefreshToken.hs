{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Req.OAuth2.Internal.RefreshToken
    ( RefreshTokenRequest(..)
    , RefreshTokenResponse(..)
    , fetchRefreshToken
    ) where

import           Data.Aeson ((.:), withObject)
import           Data.Aeson.Types (Parser, Value, parseEither)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Network.HTTP.Req ((=:))
import           Network.HTTP.Req.Url.Extra (toUrlHttps)
import           Network.HTTP.Req.OAuth2.Internal.Types
import           Network.HTTP.Req.OAuth2.Internal.Util

data RefreshTokenRequest = RefreshTokenRequest RefreshToken

data RefreshTokenResponse = RefreshTokenResponse TokenPair

fetchRefreshToken :: App -> RefreshTokenRequest -> IO (Either String RefreshTokenResponse)
fetchRefreshToken app (RefreshTokenRequest (RefreshToken rt)) = do
    let clientPair = appClientPair app
        Just (url, _) = toUrlHttps $ appTokenUri app
    parseEither pResponse <$>
        oAuth2PostRaw
            url
            (oAuth2AuthHeader clientPair)
            ("grant_type" =: ("refresh_token" :: Text) <> "refresh_token" =: rt <> "expires_in" =: ("3600" :: Text))

pResponse :: Value -> Parser RefreshTokenResponse
pResponse =
    withObject "RefreshTokenResponse" $ \v -> (RefreshTokenResponse . ) . TokenPair
        <$> (AccessToken <$> v .: "access_token")
        <*> (RefreshToken <$> v .: "refresh_token")
