{-# LANGUAGE OverloadedStrings #-}

module Main where

import Foundation
import Foundation.Conduit
import Foundation.UUID (uuidParser)
import Foundation.Parser (parseOnly)
import Foundation.IO
import Basement.Block (Block)
import Data.ByteArray (convert)
import Foundation.Format.CSV

import Control.Monad (forM_)

import Revolut.V1

import Data.Hourglass (timePrint, timeParseE, ISO8601_Date(..))

import Console.Options

main :: IO ()
main = defaultMain $ do
    programName "revolut-cli"
    programDescription "Revolut Business CLI"

    key <- flagParam (FlagShort 'k' <> FlagLong "api-key" <> FlagDescription "API Key as provided by Resvolut")
                     (FlagRequired (Right . fromString))
    host <- flagParam (FlagShort 'h' <> FlagLong "url" <> FlagDescription "Host to revolut URL")
                      (FlagRequired (Right . fromString))

    command "accounts" (commandAccounts host key)
    command "statements" (commandStatements host key)
  where
    commandAccounts host key =
        action $ \get -> do
            let k = maybe (error "need --api-key") id $ get key
            let h = maybe ( "b2b.revolut.com") id $ get host
            let cfg = Config (BaseUrl Https h 443 "/api/1.0/") k

            r <- run cfg getAccounts
            case r of
                Left err -> error (show err)
                Right l -> runConduit $
                    yields l .| mkAccounts
                             .| rowC
                             .| blockToUarrayC
                             .| sinkHandle stdout
      where
        mkAccounts = awaitForever $ \a ->
            yield ( show $ accountId a
                  , accountName a
                  , show $ accountCurrency a
                  , accountBalande a
                  )
    commandStatements host key = do
        {-
        from <- flagParam (FlagLong "from" <> FlagDescription "Filter from the given date (format: YYYY-MM-DD)")
                          (FlagRequired parseDate)
        to <- flagParam (FlagLong "to" <> FlagDescription "Filter up to the given date (format: YYYY-MM-DD)")
                        (FlagRequired parseDate)
        account <- flagParam (FlagLong "account" <> FlagDescription "Filter only for the given account (UUID)")
                             (FlagRequired parseUUID)
        -}
        action $ \get -> do
            let k = maybe (error "need --api-key") id $ get key
            let h = maybe ( "b2b.revolut.com") id $ get host
            let cfg = Config (BaseUrl Https h 443 "/api/1.0/") k

            r <- run cfg $ (,)
                    <$> getAccounts
                    <*> getTransactions Nothing Nothing Nothing Nothing Nothing -- (get from) (get to) Nothing Nothing Nothing

            case r of
                Left err -> error (show err)
                Right (as, ls) ->
                    forM_ (groupByAccounts Nothing as ls) $ \(a, ts) -> do
                        putStrLn $ "Account " <> accountName a <> " " <> show (accountCurrency a) <> " (" <> show (accountId a) <> ")"
                        runConduit $
                            yields ts .| mkLimited (accountId a) (accountBalande a)
                                      .| rowC
                                      .| blockToUarrayC
                                      .| sinkHandle stdout
          where
            mkLimited :: Monad m => UUID -> Double -> Conduit Transaction (LString, Maybe String, String, Maybe Double, Double) m ()
            mkLimited uuid balance = do
                m <- await
                case m of
                    Nothing -> pure ()
                    Just t  -> do
                        let leg = findLeg uuid (transactionLegs t)
                        let amount = maybe 0 transactionLegAmount leg
                        let balance' = balance - amount
                        yield ( timePrint ("DD/MM/YYYY" :: LString) $ transactionCreatedAt t
                              , transactionReference t
                              , show $ transactionType t
                              , transactionLegAmount <$> leg
                              , balance
                              )
                        mkLimited uuid balance'

findLeg :: UUID -> [TransactionLeg] -> Maybe TransactionLeg
findLeg _ [] = Nothing
findLeg uuid (x:xs)
    | transactionLegAccountId x == uuid = Just x
    | otherwise                         = findLeg uuid xs

groupByAccounts :: Maybe UUID -> [Account] -> [Transaction] -> [(Account, [Transaction])]
groupByAccounts _ []     _ = []
groupByAccounts f@Nothing     (a:as) l = (a, groupByAccount a l) : groupByAccounts f as l
groupByAccounts f@(Just uuid) (a:as) l
    | uuid == accountId a = (a, groupByAccount a l) : groupByAccounts f as l
    | otherwise           = groupByAccounts f as l
groupByAccount :: Account -> [Transaction] -> [Transaction]
groupByAccount _ [] = []
groupByAccount a (t:ts) = case transactionLegs t of
    [] -> groupByAccount a ts
    [x1] | transactionLegAccountId x1 == accountId a -> t : groupByAccount a ts
         | otherwise                                 -> groupByAccount a ts
    (x1:x2:_) | transactionLegAccountId x1 == accountId a -> t : groupByAccount a ts
              | transactionLegAccountId x2 == accountId a -> t : groupByAccount a ts
              | otherwise                                 -> groupByAccount a ts

parseDate :: LString -> Either LString Date
parseDate str = case timeParseE ISO8601_Date str of
    Left err -> Left $ toList $ show err
    Right (DateTime v _, _)  -> Right v

parseUUID :: LString -> Either LString UUID
parseUUID str = case parseOnly uuidParser (fromList str :: String) of
    Left err -> Left $ toList $ show err
    Right v  -> Right v

blockToUarrayC :: Monad m => Conduit (Block Word8) (UArray Word8) m ()
blockToUarrayC = awaitForever $  yield . convert
