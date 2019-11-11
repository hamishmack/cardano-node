{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Config.Protocol
  ( Protocol(..)
  , SomeProtocol(..)
  , fromProtocol
  ) where



import           Cardano.Prelude
import           Prelude (error, fail)
import           Test.Cardano.Prelude (canonicalDecodePretty)

import           Codec.CBOR.Read (deserialiseFromBytes, DeserialiseFailure)
import qualified Data.ByteString.Lazy as LB
import           Data.Text (unpack)

import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import           Cardano.Crypto (RequiresNetworkMagic, decodeHash)
import qualified Cardano.Crypto.Signing as Signing

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Mempool.API (ApplyTxErr, GenTx, GenTxId)
import           Ouroboros.Consensus.Node.ProtocolInfo (PBftLeaderCredentials,
                                                        PBftSignatureThreshold(..),
                                                         mkPBftLeaderCredentials)
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.Protocol (SecurityParam (..),
                                               PraosParams (..),
                                               PBftParams (..))
import qualified Ouroboros.Consensus.Protocol as Consensus
import qualified Ouroboros.Consensus.Ledger.Byron        as Consensus
import qualified Ouroboros.Consensus.Ledger.Byron.Config as Consensus
import           Ouroboros.Consensus.Util (Dict(..))
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Network.Block

import           Cardano.Config.Types
                   (DelegationCertFile (..), GenesisFile (..),
                    LastKnownBlockVersion (..), Update (..),
                    Protocol (..), SigningKeyFile (..))

-- TODO: consider not throwing this, or wrap it in a local error type here
-- that has proper error messages.
instance Exception Genesis.ConfigurationError

type TraceConstraints blk =
    ( Condense blk
    , Condense [blk]
    , Condense (ChainHash blk)
    , Condense (Header blk)
    , Condense (HeaderHash blk)
    , Condense (GenTx blk)
    , Show (ApplyTxErr blk)
    , Show (GenTx blk)
    , Show (GenTxId blk)
    , Show blk
    , Show (Header blk)
    )

{-------------------------------------------------------------------------------
  Untyped/typed protocol boundary
-------------------------------------------------------------------------------}


mockSecurityParam :: SecurityParam
mockSecurityParam = SecurityParam 5

data SomeProtocol where
  SomeProtocol :: (RunNode blk, TraceConstraints blk)
               => Consensus.Protocol blk -> SomeProtocol

fromProtocol
  :: Text
  -> GenesisFile
  -> RequiresNetworkMagic
  -> Maybe Double
  -> Maybe DelegationCertFile
  -> Maybe SigningKeyFile
  -> Update
  -> Protocol
  -> IO SomeProtocol

fromProtocol _ _ _ _ _ _ _ ByronLegacy =
  error "Byron Legacy protocol is not implemented."

fromProtocol _ _ _ _ _ _ _ BFT =
  case Consensus.runProtocol p of
    Dict -> return $ SomeProtocol p

  where
    p = Consensus.ProtocolMockBFT mockSecurityParam

fromProtocol _ _ _ _ _ _ _ Praos =
  case Consensus.runProtocol p of
    Dict -> return $ SomeProtocol p

  where
    p = Consensus.ProtocolMockPraos PraosParams {
        praosSecurityParam = mockSecurityParam
      , praosSlotsPerEpoch = 3
      , praosLeaderF       = 0.5
      , praosLifetimeKES   = 1000000
      }


fromProtocol _ _ _ _ _ _ _ MockPBFT =
  case Consensus.runProtocol p of
    Dict -> return $ SomeProtocol p

  where
    p = Consensus.ProtocolMockPBFT PBftParams {
        pbftSecurityParam      = mockSecurityParam
      , pbftNumNodes           = numNodes
      , pbftSignatureThreshold = (1.0 / fromIntegral numNodes) + 0.1
      }
    numNodes = 3

fromProtocol gHash genFile nMagic sigThresh delCertFp sKeyFp update RealPBFT = do
    let genHash = either panic identity $ decodeHash gHash

    gcE <- runExceptT (Genesis.mkConfigFromFile
                       nMagic
                       (unGenesisFile genFile)
                       genHash
                      )
    let gc = case gcE of
            Left err -> panic $ show err
            Right x -> x

    optionalLeaderCredentials <- readLeaderCredentials
                                   gc
                                   delCertFp
                                   sKeyFp

    let p = protocolConfigRealPbft update sigThresh gc optionalLeaderCredentials

    case Consensus.runProtocol p of
      Dict -> return $ SomeProtocol p


-- | The plumbing to select and convert the appropriate configuration subset
-- for the 'RealPBFT' protocol.
--
protocolConfigRealPbft :: Update
                       -> Maybe Double
                       -> Genesis.Config
                       -> Maybe PBftLeaderCredentials
                       -> Consensus.Protocol (Consensus.ByronBlockOrEBB
                                                Consensus.ByronConfig)
protocolConfigRealPbft (Update appName appVer lastKnownBlockVersion)
                       pbftSignatureThresh
                       genesis leaderCredentials =
    Consensus.ProtocolRealPBFT
      genesis
      (PBftSignatureThreshold <$> pbftSignatureThresh)
      (convertProtocolVersion lastKnownBlockVersion)
      (Update.SoftwareVersion (Update.ApplicationName appName)
                              (toEnum appVer))
      leaderCredentials
  where
    convertProtocolVersion
      LastKnownBlockVersion {lkbvMajor, lkbvMinor, lkbvAlt} =
      Update.ProtocolVersion (toEnum lkbvMajor)
                             (toEnum lkbvMinor)
                             (toEnum lkbvAlt)


readLeaderCredentials :: Genesis.Config
                      -> Maybe DelegationCertFile
                      -> Maybe SigningKeyFile
                      -> IO (Maybe PBftLeaderCredentials)
readLeaderCredentials gc mDelCertFp mSKeyFp = do
  case (mDelCertFp, mSKeyFp) of
    (Nothing, Nothing) -> pure Nothing
    (Just _, Nothing) -> panic "Signing key filepath not specified"
    (Nothing, Just _) -> panic "Delegation certificate filepath not specified"
    (Just delegCertFile, Just signingKeyFile) -> do
         signingKeyFileBytes <- LB.readFile $ unSigningKey signingKeyFile
         delegCertFileBytes <- LB.readFile $ unDelegationCert delegCertFile

         --TODO: review the style of reporting for input validation failures
         -- If we use throwIO, we should use a local exception type that
         -- wraps the other structured failures and reports them appropriatly
         signingKey <- either throwIO return $
                         deserialiseSigningKey signingKeyFileBytes
         delegCert  <- either (fail . unpack) return $
                         canonicalDecodePretty delegCertFileBytes
         either throwIO (return . Just)
                (mkPBftLeaderCredentials gc signingKey delegCert)

  where
    deserialiseSigningKey :: LB.ByteString
                          -> Either DeserialiseFailure Signing.SigningKey
    deserialiseSigningKey =
        fmap (Signing.SigningKey . snd)
      . deserialiseFromBytes Signing.fromCBORXPrv
