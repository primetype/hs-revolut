{-# LANGUAGE TypeApplications #-}

module Revolut.V1.GetAccounts
    ( Account(..)
    , GetAccounts
    , getAccounts
    ) where

import Foundation

import Servant.API
import Revolut.V1.Types
import Revolut.V1.Client

import Data.Aeson

getAccounts :: RevolutM [Account]
getAccounts = revolut (Proxy @GetAccounts)

type GetAccounts
    = "accounts"
    :> Get '[JSON] [Account]

data Account = Account
    { accountId :: !UUID
    , accountName :: !String
    , accountBalande :: !Double
    , accountCurrency :: !Currency
    , accountState :: !AccountState
    , accountPublic :: !Bool
    , accountCreatedAt :: !DateTime
    , accountUpdatedAt :: !DateTime
    }
  deriving (Show, Typeable)
instance FromJSON Account where
    parseJSON = withObject "Account" $ \o ->
        Account
            <$> o .: "id"
            <*> o .: "name"
            <*> o .: "balance"
            <*> o .: "currency"
            <*> o .: "state"
            <*> o .: "public"
            <*> o .: "created_at"
            <*> o .: "updated_at"
