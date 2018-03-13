{-# LANGUAGE TypeApplications #-}

module Revolut.V1.GetTransactions
    ( Transaction(..)
    , TransactionLeg(..)
    , CounterParty(..)
    , GetTransactions
    , getTransactions
    ) where

import Foundation

import Servant.API
import Revolut.V1.Types
import Revolut.V1.Client

import Data.Aeson

getTransactions :: Maybe Date
                -> Maybe Date
                -> Maybe UUID
                -> Maybe (CountOf Transaction)
                -> Maybe TransactionType
                -> RevolutM [Transaction]
getTransactions = revolut (Proxy @GetTransactions)

type GetTransactions
    = "transactions"
    :> QueryParam "from" Date
    :> QueryParam "to" Date
    :> QueryParam "counterparty" UUID
    :> QueryParam "count" (CountOf Transaction)
    :> QueryParam "type" TransactionType
    :> Get '[JSON] [Transaction]

data Transaction = Transaction
    { transactionId :: !UUID
    , transactionType :: !TransactionType
    , transactionRequestId :: !(Maybe String)
    , transactionState :: !TransactionState
    , transactionStateReason :: !(Maybe String)
    , transactionCreatedAt :: !DateTime
    , transactionUpdatedAt :: !DateTime
    , transactionCompletedAt :: !(Maybe DateTime)
    , transactionScheduledFor :: !(Maybe DateTime)
    , transactionReference :: !(Maybe String)
    , transactionLegs :: ![TransactionLeg]
    }
  deriving (Show, Eq, Typeable)
instance FromJSON Transaction where
    parseJSON = withObject "Transaction" $ \o ->
        Transaction
            <$> o .: "id"
            <*> o .: "type"
            <*> o .:? "request_id"
            <*> o .: "state"
            <*> o .:? "reason_code"
            <*> o .: "created_at"
            <*> o .: "updated_at"
            <*> o .:? "completed_at"
            <*> o .:? "scheduled_for"
            <*> o .:? "reference"
            <*> o .: "legs"

data TransactionLeg = TransactionLeg
    { transactionLegId :: !UUID
    , transactionLegAmount :: !Double
    , transactionLegCurrency :: !Currency
    , transactionLegBillAmount :: !(Maybe Double)
    , transactionLegBillCurrency :: !(Maybe Currency)
    , transactionLegAccountId :: !UUID
    , transactionLegCounterParty :: !(Maybe CounterParty)
    }
  deriving (Show, Eq, Typeable)
instance FromJSON TransactionLeg where
    parseJSON = withObject "TransactionLeg" $ \o ->
        TransactionLeg
            <$> o .: "leg_id"
            <*> o .: "amount"
            <*> o .: "currency"
            <*> o .:? "bill_amount"
            <*> o .:? "bill_currency"
            <*> o .: "account_id"
            <*> o .:? "counterparty"

data CounterParty = CounterParty
    { counterPartyId :: !UUID
    , counterPartyAccountId :: !UUID
    , counterPartyAccountType :: !AccountType
    , counterPartyDescription :: !(Maybe String)
    }
  deriving (Show, Eq, Typeable)
instance FromJSON CounterParty where
    parseJSON = withObject "CounterParty" $ \o ->
        CounterParty
            <$> o .: "id"
            <*> o .: "account_id"
            <*> o .: "account_type"
            <*> o .:? "description"
