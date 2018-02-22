{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Req.OAuth2.Util
    ( oAuth2Post
    , tokenAuthHeader
    ) where

import           Data.Aeson (Value)
import qualified Data.ByteString as ByteString (append, concat)
import qualified Data.ByteString.Base64 as Base64 (encode)
import           Data.Default.Class (def)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import           Network.HTTP.Req
                    ( FormUrlEncodedParam
                    , Option
                    , POST(..)
                    , ReqBodyUrlEnc(..)
                    , Scheme(..)
                    , Url
                    , header
                    , jsonResponse
                    , req
                    , responseBody
                    , runReq
                    )
import           Network.HTTP.Req.OAuth2.Types

oAuth2Post :: Url 'Https -> Option 'Https -> FormUrlEncodedParam -> IO Value
oAuth2Post url opts formBody =
    runReq def $
        responseBody <$> req POST url (ReqBodyUrlEnc formBody) jsonResponse opts

tokenAuthHeader :: ClientId -> ClientSecret -> Option scheme
tokenAuthHeader clientId clientSecret =
    header "Authorization" (ByteString.append "Basic " (encodeClientAuth clientId clientSecret))
    where
        encodeClientAuth (ClientId cid) (ClientSecret cs) = Base64.encode $ ByteString.concat [Text.encodeUtf8 cid, ":", Text.encodeUtf8 cs]
