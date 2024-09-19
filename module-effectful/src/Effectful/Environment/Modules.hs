{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Effectful.Environment.Modules (
  withModuleEnvs,
  parseModuleEnvVars,
  ModuleConfig (..),
  ModuleBackend (..),

  -- * Utility functions
  getCommand,
) where

import Control.DeepSeq (NFData)
import Control.Exception.Safe (Exception, bracket_, throwIO)
import Control.Lens as Lens
import Control.Lens.Extras
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import Data.Aeson.Types (ToJSON (..), parseJSON)
import Data.Binary (Binary)
import qualified Data.CaseInsensitive as CI
import Data.Generics.Labels ()
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.Monoid (Ap (..))
import Data.String (IsString (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Effectful
import Effectful.Environment
import Effectful.Error.Static (runErrorNoCallStack, throwError)
import Effectful.Process.Typed (ExitCode (..), TypedProcess, readProcess)
import GHC.Generics (Generic)
import qualified Language.Bash.Parse as Bash
import qualified Language.Bash.Pretty as Bash
import qualified Language.Bash.Syntax as Bash
import qualified Language.Bash.Word as Bash
import qualified Text.BashScript as Shell

data ModuleBackend = EnvironmentModule | LMod | Command T.Text
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable, Binary)

instance ToJSON ModuleBackend where
  toJSON EnvironmentModule = "module"
  toJSON LMod = "lmod"
  toJSON (Command cmd) = toJSON cmd

instance FromJSON ModuleBackend where
  parseJSON p = do
    txt <- J.parseJSON p
    case CI.mk txt of
      "modulecmd" -> pure EnvironmentModule
      "modules" -> pure EnvironmentModule
      "module" -> pure EnvironmentModule
      "emodule" -> pure EnvironmentModule
      "emodules" -> pure EnvironmentModule
      "emods" -> pure EnvironmentModule
      "emod" -> pure EnvironmentModule
      "environmentmodules" -> pure EnvironmentModule
      "environment_modules" -> pure EnvironmentModule
      "environment-modules" -> pure EnvironmentModule
      "envmodules" -> pure EnvironmentModule
      "env_modules" -> pure EnvironmentModule
      "env-modules" -> pure EnvironmentModule
      "env_mods" -> pure EnvironmentModule
      "env-mods" -> pure EnvironmentModule
      "envmods" -> pure EnvironmentModule
      "lmod" -> pure LMod
      _ -> pure $ Command txt

data ModuleConfig = ModuleConfig
  { command :: !ModuleBackend
  , load :: ![String]
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable, ToJSON, FromJSON)

getCommand :: ModuleBackend -> T.Text
getCommand EnvironmentModule = "modulecmd bash load"
getCommand LMod = "/usr/share/lmod/lmod/libexec/lmod bash load"
getCommand (Command cmd) = cmd

-- | Throws 'ModuleCmdException' when interaction error.
withModuleEnvs ::
  (Environment :> es, TypedProcess :> es) =>
  ModuleConfig ->
  Eff es a ->
  Eff es a
withModuleEnvs cfg act = do
  env0 <- getEnvironment
  envs <- either throwIO pure =<< parseModuleEnvVars cfg
  bracket_ (imapM_ setEnv envs) (mapM_ (uncurry setEnv) env0) act

parseModuleEnvVars ::
  ( TypedProcess :> es
  ) =>
  ModuleConfig ->
  Eff es (Either ModuleCmdException (HashMap String String))
parseModuleEnvVars modules
  | null modules.load = pure $ Right mempty
  | otherwise = runErrorNoCallStack do
      getAp $
        flip foldMap modules.load \targ -> Ap do
          (code, out, err0) <-
            readProcess $
              fromString $
                T.unpack $
                  T.strip (getCommand modules.command) <> " " <> Shell.escapeRawText (T.pack targ)
          case code of
            ec@ExitFailure {} ->
              throwError $ FailedToRunModuleCmd ec (LT.decodeUtf8 out) (LT.decodeUtf8 err0)
            ExitSuccess ->
              case Bash.parse "modulecmd" $ LT.unpack $ LT.decodeUtf8 out of
                Right (Bash.List stmts) -> do
                  let vars =
                        stmts
                          ^.. folded
                            . biplate @_ @Bash.Assign
                            . #_Assign
                            . runFold do
                              k <- Fold Lens._1
                              v <- Fold $ Lens._3 . #_RValue
                              pure (Bash.prettyText k, Bash.unquote v)
                  pure $ HM.fromList vars
                Left err -> do
                  let sout = LT.decodeUtf8 out
                      serr = LT.decodeUtf8 err0
                  throwError $ InvalidBashScript InvalidBashScriptInfo {modules, parseError = show err, stdout = sout, stderr = serr}

data ModuleCmdException
  = FailedToRunModuleCmd !ExitCode !LT.Text !LT.Text
  | InvalidBashScript InvalidBashScriptInfo
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Exception)

data InvalidBashScriptInfo = InvalidBashScriptInfo
  { modules :: !ModuleConfig
  , parseError :: !String
  , stdout, stderr :: LT.Text
  }
  deriving (Show, Eq, Ord, Generic)
