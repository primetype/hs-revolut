{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Revolut.V1.Client
    ( ApiKey
    , Config(..)

    , RevolutM
    , revolut
    , run
    ) where

import Foundation
import Foundation.Monad
import Foundation.Monad.Reader

import Basement.NormalForm ()

import Data.ByteArray (convert)

import Servant.Client hiding (manager)

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Data.Text.Encoding  (decodeUtf8)
import Servant.Client.Core


-- | Revolut Business 'ApiKey'
--
-- generated on your account at https://business.revolut.com/settings/api
--
-- Use Foundation's 'IsString' functions for convenient conversions.
--
newtype ApiKey = ApiKey String
  deriving (Typeable, NormalForm, IsString)

data Config = Config
    { cfgBaseUrl :: !BaseUrl
    , cfgApiKey  :: !ApiKey
    }

newtype RevolutM a = RevolutM { runRevolutM_ :: ReaderT Config ClientM a }
  deriving ( Functor, Applicative, Monad, MonadIO, Typeable)
instance MonadReader RevolutM where
    type ReaderContext RevolutM = Config
    ask = RevolutM ask

runRevolutM :: Config -> RevolutM a -> ClientM a
runRevolutM cfg m = runReaderT (runRevolutM_ m) cfg

run :: Config -> RevolutM a -> IO (Either ServantError a)
run cfg m = do
    manager <- newManager tlsManagerSettings
    runClientM (runRevolutM cfg m) (ClientEnv manager (cfgBaseUrl cfg) Nothing)

revolut :: HasClient RevolutM api => Proxy api -> Client RevolutM api
revolut p = clientIn p (Proxy @RevolutM)

revolutAuthReq :: ApiKey -> Request -> Request
revolutAuthReq (ApiKey str) req =
    let authText = decodeUtf8 $ convert $ "Bearer " <> str
    in addHeader "Authorization" authText req

instance RunClient RevolutM where
    runRequest req = do
        val <- cfgApiKey <$> ask
        let req' = revolutAuthReq val req
        RevolutM $ lift $ runRequest req'
    streamingRequest req = do
        val <- cfgApiKey <$> ask
        let req' = revolutAuthReq val req
        RevolutM $ lift $ streamingRequest req'
    throwServantError = RevolutM . lift . throwServantError
    catchServantError ma handler = do
        cfg <- ask
        RevolutM $ lift $
            catchServantError (runRevolutM cfg ma)
                              (runRevolutM cfg . handler)
