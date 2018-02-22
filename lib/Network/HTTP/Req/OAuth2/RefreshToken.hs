{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Req.OAuth2.RefreshToken
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
import           Network.HTTP.Req.OAuth2.App
import           Network.HTTP.Req.OAuth2.Types
import           Network.HTTP.Req.OAuth2.Util

data RefreshTokenRequest = RefreshTokenRequest ClientPair RefreshToken

data RefreshTokenResponse = RefreshTokenResponse TokenPair

fetchRefreshToken :: App -> RefreshTokenRequest -> IO (Either String RefreshTokenResponse)
fetchRefreshToken app (RefreshTokenRequest clientPair (RefreshToken rt)) = do
    let Just (url, _) = toUrlHttps $ tokenUri app
    parseEither pResponse <$>
        oAuth2Post
            url
            (oAuth2AuthHeader clientPair)
            ("grant_type" =: ("refresh_token" :: Text) <> "refresh_token" =: rt <> "expires_in" =: ("3600" :: Text))

pResponse :: Value -> Parser RefreshTokenResponse
pResponse =
    withObject "RefreshTokenResponse" $ \v -> (RefreshTokenResponse . ) . TokenPair
        <$> (AccessToken <$> v .: "access_token")
        <*> (RefreshToken <$> v .: "refresh_token")
