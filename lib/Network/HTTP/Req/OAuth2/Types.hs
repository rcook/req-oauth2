module Network.HTTP.Req.OAuth2.Types
    ( AccessToken(..)
    , ClientId(..)
    , ClientPair(..)
    , ClientSecret(..)
    , RefreshToken(..)
    , TokenPair(..)
    ) where

import           Data.Text (Text)

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
