module Network.HTTP.Req.OAuth2.App
    ( App(..)
    ) where

import           Text.URI (URI)

data App = App
    { authUri :: URI
    , tokenUri :: URI
    }
