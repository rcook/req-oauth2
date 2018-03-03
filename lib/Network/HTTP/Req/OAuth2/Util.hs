{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Network.HTTP.Req.OAuth2.Util
    ( acceptLanguage
    , evalOAuth2
    , hasResponseStatus
    , oAuth2AuthHeader
    , oAuth2BearerHeader
    , oAuth2PostRaw
    , runOAuth2
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.State.Strict (evalStateT, get, put, runStateT)
import           Data.Aeson (Value)
import qualified Data.ByteString as ByteString (append, concat)
import qualified Data.ByteString.Base64 as Base64 (encode)
import           Data.Default.Class (def)
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
                    , header
                    , jsonResponse
                    , oAuth2Bearer
                    , req
                    , responseBody
                    , runReq
                    )
import           Network.HTTP.Req.OAuth2.Types
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
    runReq def $
        responseBody <$> req POST url (ReqBodyUrlEnc formBody) jsonResponse opts

evalOAuth2 :: App -> TokenPair -> ((forall b . APICall b) -> OAuth2 a) -> IO a
evalOAuth2 app tokenPair f = flip evalStateT tokenPair (f $ mkCall app)

runOAuth2 :: App -> TokenPair -> ((forall b . APICall b) -> OAuth2 a) -> IO (a, TokenPair)
runOAuth2 app tokenPair f = flip runStateT tokenPair (f $ mkCall app)

mkCall :: App -> APICall a
mkCall app action = do
    tp <- get
    (result, tp') <- liftIO $ action app tp
    put tp'
    return result
