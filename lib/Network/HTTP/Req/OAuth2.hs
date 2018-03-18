{-|
Module      : Network.HTTP.Req.OAuth2
Description : Basic OAuth2 support for Req
Copyright   : (C) Richard Cook, 2018
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable

This package provides basic support for OAuth2 authentication for <https://hackage.haskell.org/package/req Req>.
-}

module Network.HTTP.Req.OAuth2
    ( AccessToken(..)
    , AccessTokenRequest(..)
    , AccessTokenResponse(..)
    , App(..)
    , ClientId(..)
    , ClientPair(..)
    , ClientSecret(..)
    , OAuth2
    , PromptForCallbackUri
    , RefreshToken(..)
    , TokenPair(..)
    , UpdateTokenPair
    , evalOAuth2
    , fetchAccessToken
    , getAuthCode
    , oAuth2Get
    , runOAuth2
    ) where

import Network.HTTP.Req.OAuth2.Internal.AccessToken
import Network.HTTP.Req.OAuth2.Internal.AuthCode
import Network.HTTP.Req.OAuth2.Internal.RefreshToken
import Network.HTTP.Req.OAuth2.Internal.Types
import Network.HTTP.Req.OAuth2.Internal.Util
import Network.HTTP.Req.OAuth2.Internal.Verbs
