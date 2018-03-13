{-# OPTIONS_GHC -Wno-orphans #-}

module Revolut.V1.Types
    ( Date(..)
    , DateTime(..)
    , Month(..)
    , TimeOfDay(..)
    , UUID
    , TransactionType(..)
    , TransactionState(..)
    , Currency(..)
    , AccountType(..)
    , AccountState(..)
    ) where

import Foundation
import Foundation.UUID
import Foundation.String (fromBytes, Encoding(UTF8))
import Foundation.Parser (parseOnly)

import Data.Aeson

import Data.Hourglass (Date(..), DateTime(..), TimeOfDay(..), Month(..))
import qualified Data.Hourglass as H
import Web.Internal.HttpApiData (ToHttpApiData(..))

import Data.ByteArray (convert)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text (pack, unpack)

instance ToHttpApiData Date where
    toUrlPiece = pack . H.timePrint H.ISO8601_Date
instance FromJSON Date where
    parseJSON v = do
        str <- parseJSON v
        case H.timeParseE H.ISO8601_Date str of
            Left err -> fail (toList $ show err)
            Right (DateTime d _, _) -> pure d

dateTimeFormat :: LString
dateTimeFormat = "YYYY-MM-DDTH:MI:S.p3Z"

instance ToHttpApiData DateTime where
    toUrlPiece = pack . H.timePrint dateTimeFormat
instance FromJSON DateTime where
    parseJSON v = do
        str <- parseJSON v
        case H.timeParseE dateTimeFormat str of
            Left err -> fail (toList $ show err)
            Right (dt, _) -> pure dt

instance ToHttpApiData UUID where
    toUrlPiece = decodeUtf8 . convert . show
instance FromJSON UUID where
    parseJSON = withText "UUID" $ \t ->
        let (str, _, _) = (fromBytes UTF8 $ convert $ encodeUtf8 t)
         in case parseOnly uuidParser str of
                Left err -> fail (toList $ show err)
                Right uuid -> pure uuid

instance FromJSON String where
    parseJSON = withText "UTF8 String" $ \t ->
        let (str, _, _) = (fromBytes UTF8 $ convert $ encodeUtf8 t)
         in pure str

instance ToHttpApiData (CountOf a) where
    toUrlPiece = toUrlPiece . fromCount
instance FromJSON (CountOf a) where
    parseJSON obj = toCount <$> parseJSON obj

-- the transaction type, one of atm, card_payment,  card_refund, card_chargeback
-- , card_credit, exchange,  transfer, loan, fee, refund, topup, topup_return
-- , tax,  tax_refund
data TransactionType
    = TxATM
    | TxCardPayment
    | TxCardRefund
    | TxCardChargeBack
    | TxCardCredit
    | TxExchange
    | TxTransfer
    | TxLoan
    | TxFee
    | TxRefund
    | TxTopup
    | TxTopupReturn
    | TxTax
    | TxTaxRefund
  deriving (Eq, Ord, Show, Typeable, Enum, Bounded)
instance ToHttpApiData TransactionType where
    toUrlPiece TxATM = "atm"
    toUrlPiece TxCardPayment = "card_payment"
    toUrlPiece TxCardRefund = "card_refund"
    toUrlPiece TxCardChargeBack = "card_chargeback"
    toUrlPiece TxCardCredit = "card_credit"
    toUrlPiece TxExchange = "exchange"
    toUrlPiece TxTransfer = "transfer"
    toUrlPiece TxLoan = "loan"
    toUrlPiece TxFee = "fee"
    toUrlPiece TxRefund = "refund"
    toUrlPiece TxTopup = "topup"
    toUrlPiece TxTopupReturn = "topup_return"
    toUrlPiece TxTax = "tax"
    toUrlPiece TxTaxRefund = "tax_refund"
instance FromJSON TransactionType where
    parseJSON = withText "TransactionType" $ \t -> case t of
        "atm" -> pure TxATM
        "card_payment" -> pure TxCardPayment
        "card_refund" -> pure TxCardRefund
        "card_chargeback" -> pure TxCardChargeBack
        "card_credit" -> pure TxCardCredit
        "exchange" -> pure TxExchange
        "transfer" -> pure TxTransfer
        "loan" -> pure TxLoan
        "fee" -> pure TxFee
        "refund" -> pure TxRefund
        "topup" -> pure TxTopup
        "topup_return" -> pure TxTopupReturn
        "tax" -> pure TxTax
        "tax_refund" -> pure TxTaxRefund
        _ -> fail (unpack t)

data TransactionState
    = TxPending
    | TxCompleted
    | TxDeclined
    | TxFailed
  deriving (Eq, Ord, Show, Typeable, Enum, Bounded)
instance FromJSON TransactionState where
    parseJSON = withText "TransactionState" $ \t -> case t of
        "pending" -> pure TxPending
        "completed" -> pure TxCompleted
        "declined" -> pure TxDeclined
        "failed" -> pure TxFailed
        _ -> fail (unpack t)

data Currency = GBP | EUR | USD
  deriving (Eq, Ord, Show, Typeable, Enum, Bounded)
instance FromJSON Currency where
    parseJSON = withText "Currency" $ \t -> case t of
        "GBP" -> pure GBP
        "EUR" -> pure EUR
        "USD" -> pure USD
        _ -> fail (unpack t)

data AccountType = AccountSelf | AccountRevolut | AccountExternal
  deriving (Eq, Ord, Show, Typeable, Enum, Bounded)
instance FromJSON AccountType where
    parseJSON = withText "AccountType" $ \t -> case t of
        "self" -> pure AccountSelf
        "revolut" -> pure AccountRevolut
        "external" -> pure AccountExternal
        _ -> fail (unpack t)

data AccountState = AccountActive | AccountInactive
  deriving (Eq, Ord, Show, Typeable, Enum, Bounded)
instance FromJSON AccountState where
    parseJSON = withText "AccountState" $ \t -> case t of
        "active" -> pure AccountActive
        "inactive" -> pure AccountInactive
        _ -> fail (unpack t)
