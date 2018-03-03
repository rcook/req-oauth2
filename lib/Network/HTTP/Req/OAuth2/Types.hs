{-# LANGUAGE DataKinds #-}

module Network.HTTP.Req.OAuth2.Types
    ( APIAction
    , APICall
    , APIResult
    , AccessToken(..)
    , App(..)
    , ClientId(..)
    , ClientPair(..)
    , ClientSecret(..)
    , OAuth2
    , RefreshToken(..)
    , TokenPair(..)
    , UpdateTokenPair
    ) where

import           Control.Monad.Trans.State.Strict (StateT)
import           Data.Text (Text)
import           Network.HTTP.Req (Scheme(..), Url)
import           Text.URI (URI)

-- | OAuth2 application monad
type OAuth2 = StateT TokenPair IO

-- | OAuth2 client ID
newtype ClientId = ClientId Text deriving (Eq, Show)

-- | OAuth2 client secret
newtype ClientSecret = ClientSecret Text deriving (Eq, Show)

-- | OAuth2 access token
newtype AccessToken = AccessToken Text deriving Show

-- | OAuth2 refresh token
newtype RefreshToken = RefreshToken Text deriving Show

-- | OAuth2 client ID/client secret pair
data ClientPair = ClientPair ClientId ClientSecret deriving (Eq, Show)

-- | OAuth2 access/refresh token pair
data TokenPair = TokenPair AccessToken RefreshToken deriving Show

-- | Action invoked in response to update to access/refresh token pair
type UpdateTokenPair = TokenPair -> IO ()

-- | Result of a web request
type APIResult a = Either String a

-- | A web API application
data App = App
    { appAuthUri :: URI
    , appTokenUri :: URI
    , appUpdateTokenPair :: UpdateTokenPair
    , appClientPair :: ClientPair
    }

-- | Web request action
type APIAction a =
    App
    -> TokenPair
    -> IO (APIResult a, TokenPair)

-- | TODO
type APICall a = APIAction a -> OAuth2 (APIResult a)
