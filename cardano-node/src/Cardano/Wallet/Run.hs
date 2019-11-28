{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Wallet.Run
  ( WalletCLI(..)
  , runClient
  ) where

import           Cardano.Prelude

import           Data.Functor.Contravariant (contramap)
import           Data.Text (Text, pack)
import           System.Exit (exitFailure)

import           Cardano.BM.Data.Tracer (ToLogObject (..))
import           Cardano.BM.Trace (Trace, appendName)
import           Cardano.Config.Protocol(SomeProtocol(..), fromProtocol)

import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))

import           Cardano.Config.Protocol (ProtocolInstantiationError)
import           Cardano.Config.Types (ConfigYamlFilePath(..), MiscellaneousFilepaths(..),
                                       NodeCLI(..), NodeConfiguration(..), parseNodeConfiguration)
import           Cardano.Wallet.Client

runClient :: WalletCLI -> Trace IO Text -> IO ()
runClient WalletCLI{ waNodeCli , waGenesisHash} tracer = do
    nc <- parseNodeConfiguration . unConfigPath $ realConfigFp waNodeCli
    let coreNodeId = case ncNodeId nc of
                       Nothing -> panic "Cardano.Wallet.Run.runClient: NodeId not specified"
                       Just (CoreId num) -> CoreNodeId num
                       Just (RelayId _) -> panic "Cardano.Wallet.Run.runClient: Relay nodes not supported"

    let tracer' = contramap pack . toLogObject $
          appendName ("Wallet " <> pack (show coreNodeId)) tracer

    eSomeProtocol <- runExceptT $ fromProtocol
                                    waGenesisHash
                                    (ncNodeId nc)
                                    (ncNumCoreNodes nc)
                                    (genesisFile $ realMscFp waNodeCli)
                                    (ncReqNetworkMagic nc)
                                    (ncPbftSignatureThresh nc)
                                    (delegCertFile $ realMscFp waNodeCli)
                                    (signKeyFile $ realMscFp waNodeCli)
                                    (ncUpdate nc)
                                    (ncProtocol nc)

    SomeProtocol p <- case eSomeProtocol of
                        Left err -> (putTextLn $ renderError err) >> exitFailure
                        Right (SomeProtocol p) -> pure $ SomeProtocol p

    let socketDir = socketFile $ realMscFp waNodeCli
    runWalletClient p socketDir coreNodeId tracer'

data WalletCLI = WalletCLI { waNodeCli :: !NodeCLI
                           , waGenesisHash :: !Text
                           }

renderError :: ProtocolInstantiationError -> Text
renderError = pack . show
