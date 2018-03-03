module Network.HTTP.Req.OAuth2.App
    ( App(..)
    , OAuth2
    , evalOAuth2
    , runOAuth2
    ) where

import           Control.Monad.Trans.State.Strict (evalStateT, runStateT)
import           Network.HTTP.Req.OAuth2.Types
import           Text.URI (URI)

data App = App
    { authUri :: URI
    , tokenUri :: URI
    }

evalOAuth2 :: TokenPair -> OAuth2 a -> IO a
evalOAuth2 = flip evalStateT

runOAuth2 :: TokenPair -> OAuth2 a -> IO (a, TokenPair)
runOAuth2 = flip runStateT
