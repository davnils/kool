{-# LANGUAGE BangPatterns, DeriveGeneric, OverloadedStrings, TemplateHaskell #-}

module Types where

-- | Name used in registry lookup for server providing compilations
buildRegName :: String
buildRegName = "build_server"
