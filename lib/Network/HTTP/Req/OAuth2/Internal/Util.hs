{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Network.HTTP.Req.OAuth2.Internal.Util
    ( acceptLanguage
    , evalOAuth2
    , hasResponseStatus
    , oAuth2AuthHeader
    , oAuth2BearerHeader
    , oAuth2PostRaw
    , runOAuth2
    ) where

import           Control.Monad.Trans.State.Strict (evalStateT, runStateT)
import           Data.Aeson (Value)
import qualified Data.ByteString as ByteString (append, concat)
import qualified Data.ByteString.Base64 as Base64 (encode)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Network.HTTP.Client as HTTP (HttpException(..), HttpExceptionContent(..), responseStatus)
import           Network.HTTP.Req
                    ( FormUrlEncodedParam
                    , HttpException(..)
                    , Option
                    , POST(..)
                    , ReqBodyUrlEnc(..)
                    , Scheme(..)
                    , Url
                    , defaultHttpConfig
                    , header
                    , jsonResponse
                    , oAuth2Bearer
                    , req
                    , responseBody
                    , runReq
                    )
import           Network.HTTP.Req.OAuth2.Internal.Types
import           Network.HTTP.Types (Status)

hasResponseStatus :: HttpException -> Status -> Bool
hasResponseStatus
    (VanillaHttpException (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException response _))) status =
    HTTP.responseStatus response == status
hasResponseStatus _ _ = False

acceptLanguage :: Option scheme
acceptLanguage = header "Accept-Language" "en_US"

oAuth2AuthHeader :: ClientPair -> Option scheme
oAuth2AuthHeader clientPair =
    header "Authorization" (ByteString.append "Basic " (encodeClientAuth clientPair))
    where
        encodeClientAuth (ClientPair (ClientId cid) (ClientSecret cs)) = Base64.encode $ ByteString.concat [Text.encodeUtf8 cid, ":", Text.encodeUtf8 cs]

oAuth2BearerHeader :: AccessToken -> Option 'Https
oAuth2BearerHeader (AccessToken at) = oAuth2Bearer (Text.encodeUtf8 at)

oAuth2PostRaw :: Url 'Https -> Option 'Https -> FormUrlEncodedParam -> IO Value
oAuth2PostRaw url opts formBody =
    runReq defaultHttpConfig $
        responseBody <$> req POST url (ReqBodyUrlEnc formBody) jsonResponse opts

evalOAuth2 :: TokenPair -> OAuth2 a -> IO a
evalOAuth2 = flip evalStateT

runOAuth2 :: TokenPair -> OAuth2 a -> IO (a, TokenPair)
runOAuth2 = flip runStateT
