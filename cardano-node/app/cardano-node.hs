{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import           Cardano.Prelude hiding (option)
import           Prelude (String)

import           Data.Semigroup ((<>))
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt

import           Cardano.Shell.Lib (runCardanoApplicationWithFeatures)
import           Cardano.Shell.Types (CardanoApplication (..),
                                      CardanoFeature (..),)

import           Cardano.Common.Help
import           Cardano.Common.TopHandler
import           Cardano.Common.Parsers
import           Cardano.Config.Logging (createLoggingFeature)
import           Cardano.Config.Types
import           Cardano.Node.Features.Node

main :: IO ()
main = toplevelExceptionHandler $ do

    cli <- Opt.customExecParser p opts

    (features, nodeLayer) <- initializeAllFeatures cli env

    runCardanoApplicationWithFeatures features (cardanoApplication nodeLayer)

    where
      p = Opt.prefs Opt.showHelpOnEmpty

      env :: CardanoEnvironment
      env = NoEnvironment

      cardanoApplication :: NodeLayer -> CardanoApplication
      cardanoApplication = CardanoApplication . nlRunNode

      opts :: Opt.ParserInfo NodeProtocolMode
      opts =
        Opt.info (nodeProtocolModeParser
                    <**> helperBrief "help-tracing" "Show help for tracing options" cliHelpTracing
                 )

          ( Opt.fullDesc <>
            Opt.progDesc "Start node of the Cardano blockchain."
          )

      helperBrief :: String -> String -> String -> Parser (a -> a)
      helperBrief l d helpText = Opt.abortOption (Opt.InfoMsg helpText) $ mconcat
        [ Opt.long l
        , Opt.help d ]

      cliHelpTracing :: String
      cliHelpTracing = renderHelpDoc 80 $
        "Additional tracing options:"
        <$$> ""
        <$$> parserHelpOptions cliTracingParser




initializeAllFeatures
  :: NodeProtocolMode
  -> CardanoEnvironment
  -> IO ([CardanoFeature], NodeLayer)
initializeAllFeatures nCli@NodeCLI { configFp = ncFp }
                       cardanoEnvironment = do
  (loggingLayer, loggingFeature) <- createLoggingFeature cardanoEnvironment rnCli

  nodeConfig <- parseNodeConfiguration $ unConfigPath ncFp
  (nodeLayer   , nodeFeature)    <-
    createNodeFeature
      loggingLayer
      cardanoEnvironment
      nodeConfig
      rnCli

  pure ([ loggingFeature
        , nodeFeature
        ] :: [CardanoFeature]
       , nodeLayer)

initializeAllFeatures nCli@NodeCLI { configFp = ncFp }
                       cardanoEnvironment = do
    (loggingLayer, loggingFeature) <- createLoggingFeature cardanoEnvironment mnCli

    nodeConfig <- parseNodeConfiguration $ unConfigPath ncFp
    (nodeLayer   , nodeFeature)    <-
      createNodeFeature
        loggingLayer
        cardanoEnvironment
        nodeConfig
        mnCli

    pure ([ loggingFeature
          , nodeFeature
          ] :: [CardanoFeature]
         , nodeLayer)
