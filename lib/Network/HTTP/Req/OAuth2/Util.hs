{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Req.OAuth2.Util
    ( oAuth2AuthHeader
    , oAuth2BearerHeader
    , oAuth2Post
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
                    , oAuth2Bearer
                    , req
                    , responseBody
                    , runReq
                    )
import           Network.HTTP.Req.OAuth2.Types

oAuth2AuthHeader :: ClientPair -> Option scheme
oAuth2AuthHeader clientPair =
    header "Authorization" (ByteString.append "Basic " (encodeClientAuth clientPair))
    where
        encodeClientAuth (ClientPair (ClientId cid) (ClientSecret cs)) = Base64.encode $ ByteString.concat [Text.encodeUtf8 cid, ":", Text.encodeUtf8 cs]

oAuth2BearerHeader :: AccessToken -> Option 'Https
oAuth2BearerHeader (AccessToken at) = oAuth2Bearer (Text.encodeUtf8 at)

oAuth2Post :: Url 'Https -> Option 'Https -> FormUrlEncodedParam -> IO Value
oAuth2Post url opts formBody =
    runReq def $
        responseBody <$> req POST url (ReqBodyUrlEnc formBody) jsonResponse opts
