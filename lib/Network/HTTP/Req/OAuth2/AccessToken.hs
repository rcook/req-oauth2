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
import           Network.HTTP.Req.OAuth2.AuthCode
import           Network.HTTP.Req.OAuth2.Types
import           Network.HTTP.Req.OAuth2.Util
import           Network.HTTP.Req.Url.Extra (toUrlHttps)

data AccessTokenRequest = AccessTokenRequest AuthCode

data AccessTokenResponse = AccessTokenResponse TokenPair

-- | Gets OAuth2 access token
--
-- Implements standard OAuth2 access token workflow for web server apps
-- as described <https://aaronparecki.com/oauth-2-simplified/#web-server-apps here>.
--
-- We don't pass @client_secret@ because that would be silly. We also don't bother
-- with @redirect_uri@ since this do not seem to be required.
fetchAccessToken :: App -> AccessTokenRequest -> IO (Either String AccessTokenResponse)
fetchAccessToken app (AccessTokenRequest (AuthCode ac)) = do
    let clientPair = appClientPair app
        ClientPair (ClientId cid) _ = clientPair
        Just (url, _) = toUrlHttps $ appTokenUri app
    parseEither pResponse <$>
        oAuth2PostRaw
            url
            (oAuth2AuthHeader clientPair)
            ("code" =: ac <> "grant_type" =: ("authorization_code" :: Text) <> "client_id" =: cid <> "expires_in" =: ("3600" :: Text))

pResponse :: Value -> Parser AccessTokenResponse
pResponse =
    withObject "AccessTokenResponse" $ \v -> (AccessTokenResponse .) . TokenPair
        <$> (AccessToken <$> v .: "access_token")
        <*> (RefreshToken <$> v .: "refresh_token")
