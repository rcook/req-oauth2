{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Req.OAuth2.AccessToken
    ( AccessTokenRequest(..)
    , AccessTokenResponse(..)
    , fetchAccessToken
    ) where

import           Data.Aeson ((.:), withObject)
import           Data.Aeson.Types (Parser, Value, parseEither)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Network.HTTP.Req ((=:))
import           Network.HTTP.Req.OAuth2.App
import           Network.HTTP.Req.OAuth2.AuthCode
import           Network.HTTP.Req.OAuth2.Types
import           Network.HTTP.Req.OAuth2.Util
import           Network.HTTP.Req.Url.Extra (toUrlHttps)

data AccessTokenRequest = AccessTokenRequest ClientId ClientSecret AuthCode

data AccessTokenResponse = AccessTokenResponse AccessToken RefreshToken

-- | Gets OAuth2 access token
--
-- Implements standard OAuth2 access token workflow for web server apps
-- as described <https://aaronparecki.com/oauth-2-simplified/#web-server-apps here>.
--
-- We don't pass @client_secret@ because that would be silly. We also don't bother
-- with @redirect_uri@ since this do not seem to be required.
fetchAccessToken :: App -> AccessTokenRequest -> IO (Either String AccessTokenResponse)
fetchAccessToken app (AccessTokenRequest clientId@(ClientId cid) clientSecret (AuthCode ac)) = do
    let Just (url, _) = toUrlHttps $ tokenUri app
    parseEither pResponse <$>
        oAuth2Post
            url
            (tokenAuthHeader clientId clientSecret)
            ("code" =: ac <> "grant_type" =: ("authorization_code" :: Text) <> "client_id" =: cid <> "expires_in" =: ("3600" :: Text))

pResponse :: Value -> Parser AccessTokenResponse
pResponse =
    withObject "AccessTokenResponse" $ \v -> AccessTokenResponse
        <$> (AccessToken <$> v .: "access_token")
        <*> (RefreshToken <$> v .: "refresh_token")
