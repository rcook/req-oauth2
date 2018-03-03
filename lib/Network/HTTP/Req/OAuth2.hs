{-|
Module      : Network.HTTP.Req.OAuth2
Description : Basic OAuth2 support for Req
Copyright   : (C) Richard Cook, 2018
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable

This package provides basic support of OAuth2 authentication for <https://hackage.haskell.org/package/req Req>.
-}

module Network.HTTP.Req.OAuth2
    ( module Network.HTTP.Req.OAuth2.AccessToken
    , module Network.HTTP.Req.OAuth2.AuthCode
    , module Network.HTTP.Req.OAuth2.RefreshToken
    , module Network.HTTP.Req.OAuth2.Types
    , module Network.HTTP.Req.OAuth2.Util
    , module Network.HTTP.Req.OAuth2.Verbs
    ) where

import Network.HTTP.Req.OAuth2.AccessToken
import Network.HTTP.Req.OAuth2.AuthCode
import Network.HTTP.Req.OAuth2.RefreshToken
import Network.HTTP.Req.OAuth2.Types
import Network.HTTP.Req.OAuth2.Util
import Network.HTTP.Req.OAuth2.Verbs
