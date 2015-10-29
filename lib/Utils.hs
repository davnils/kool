{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Utils where

import           Control.DeepSeq                                    --(force, ($!))
import           Control.Exception                                  (Exception(..), catch, IOException, SomeException)
import           Data.Monoid                                     ((<>))
import qualified Data.Text                                       as T
import           System.Exit                                        (ExitCode(..))
import qualified System.Process                                  as Proc

instance NFData ExitCode where
  rnf (ExitFailure n) = rnf n
  rnf ExitSuccess     = ()

forceM :: (Monad m, NFData a) => m a -> m a
forceM m = m >>= (return $!) . force

invokeLocalCompiler :: [String] -> T.Text -> IO (ExitCode, String, String)
invokeLocalCompiler flags input = do
  flip Control.Exception.catch handler . forceM $ do
    putStrLn $ "[*] Invoking g++ with flags=" <> show flags
    Proc.readProcessWithExitCode "/usr/bin/g++" flags (T.unpack input)
  where
  handler (e :: IOException) = return $! force (ExitFailure 1, "", show e)
