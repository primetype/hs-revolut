module Main where

import Foundation
import Foundation.Conduit
import Foundation.Collection ((!))
import Foundation.IO
import Basement.Block (Block)
import Data.ByteArray (convert)
import Foundation.Format.CSV

import Revolut.V1
import Servant.Client (BaseUrl(..), Scheme(..))

import Data.Hourglass (timePrint)

main :: IO ()
main = do
    let cfg = Config (BaseUrl Https "b2b.revolut.com" 443 "/api/1.0/") "undefined"

    let from = Date 2018 March 01
    let to = Date 2018 March 13

    r <- run cfg $ (,)
            <$> getAccounts
            <*> getTransactions (Just from) (Just to) Nothing Nothing Nothing

    case r of
        Left err -> error (show err)
        Right (a, l) -> runConduit $ do
            let account = maybe (error "account not found") id $ find ((==) GBP . accountCurrency) a
            yields l .| mkLimited (accountBalande account)
                     .| rowC
                     .| blockToUarrayC
                     .| sinkHandle stdout
  where
    blockToUarrayC :: Monad m => Conduit (Block Word8) (UArray Word8) m ()
    blockToUarrayC = awaitForever $  yield . convert
    mkLimited :: Monad m => Double -> Conduit Transaction (LString, Maybe String, String, Maybe Double, Double) m ()
    mkLimited balance = do
        m <- await
        case m of
            Nothing -> pure ()
            Just t  -> do
                let legs = transactionLegs t
                    leg = legs ! 0
                let amount = maybe 0 transactionLegAmount leg
                if (transactionLegCurrency <$> leg) == Just GBP
                    then do
                        let balance' = balance - amount
                        yield ( timePrint ("DD/MM/YYYY" :: LString) $ transactionCreatedAt t
                              , transactionReference t
                              , show $ transactionType t
                              , transactionLegAmount <$> leg
                              , balance
                              )
                        mkLimited balance'
                    else
                        mkLimited balance
