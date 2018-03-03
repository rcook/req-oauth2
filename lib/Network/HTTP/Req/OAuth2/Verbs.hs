{-# LANGUAGE DataKinds #-}

module Network.HTTP.Req.OAuth2.Verbs
    ( oAuth2Get
    ) where

import           Control.Exception (catch, throwIO)
import           Data.Aeson (Value)
import           Data.Aeson.Types (Parser, parseEither)
import           Data.Default.Class (def)
import           Data.Monoid ((<>))
import           Network.HTTP.Req
                    ( GET(..)
                    , NoReqBody(..)
                    , Scheme(..)
                    , Url
                    , jsonResponse
                    , req
                    , responseBody
                    , runReq
                    )
import           Network.HTTP.Req.OAuth2.RefreshToken
import           Network.HTTP.Req.OAuth2.Types
import           Network.HTTP.Req.OAuth2.Util
import           Network.HTTP.Types (unauthorized401)

oAuth2Get ::
    (Value -> Parser a)
    -> Url 'Https
    -> APIAction a
oAuth2Get p apiUrl app tokenPair@(TokenPair accessToken _) = do
    (temp, tokenPair') <- catch (getHelper apiUrl accessToken >>= \value -> return (value, tokenPair)) $
                            \e -> if hasResponseStatus e unauthorized401
                                    then do
                                        newTokenPair@(TokenPair newAccessToken _) <- refreshHelper app tokenPair
                                        result <- getHelper apiUrl newAccessToken
                                        return (result, newTokenPair)
                                    else throwIO e
    return (parseEither p temp, tokenPair')

getHelper ::
    Url 'Https
    -> AccessToken
    -> IO Value
getHelper url accessToken =
    responseBody <$> (runReq def $ req GET url NoReqBody jsonResponse (oAuth2BearerHeader accessToken <> acceptLanguage))

refreshHelper ::
    App
    -> TokenPair
    -> IO TokenPair
refreshHelper app@(App _ _ u _) (TokenPair _ refreshToken) = do
    result <- fetchRefreshToken app (RefreshTokenRequest refreshToken)
    let (RefreshTokenResponse newTokenPair) = case result of
                                                        Left e -> error e -- TODO: Error handling
                                                        Right x -> x
    u newTokenPair
    return newTokenPair