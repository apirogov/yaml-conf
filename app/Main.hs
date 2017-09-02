{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies #-}
module Main where

import Data.Monoid
import Options.Applicative
import Data.Default
import Data.Yaml.Conf

data SubConf = SubConf {
    _uno :: Int
  , _dos :: String
  } deriving (Show)


data TestConf = TestConf {
    _foo :: Bool
  , _bar :: SubConf
  } deriving (Show)

---- only four magic lines required per Conf type:
instance Default SubConf where
  def = SubConf 0 "default"
makeConfig ''SubConf
deriveConfigJSON ''SubConf
----
instance Default TestConf where
  def = TestConf False def
makeConfig ''TestConf
deriveConfigJSON ''TestConf
----

-- then provide a parser that uses the specified default values and you're ready!
parseArgs :: Parser TestConf
parseArgs = TestConf
  <$> switch (long "foo" <> short 'f' <> help "foo blub")
  <*> (SubConf
        <$> option auto (long "uno" <> short 'u' <> value (_uno def))
        <*> option str  (long "dos" <> short 'd' <> value (_dos def))
      )
argsParser = info (parseArgs <**> helper) (fullDesc <> progDesc "yaml-conf demo")

main :: IO ()
main = do
  let progname = "yaml-conf-exe"
  let confname = "config"
  putStrLn "Default Conf:"
  print (def::TestConf)
  putStrLn "Default conf file locations (descending priority):"
  print =<< defaultConfSources progname confname
  putStrLn "Configuration Diffs (ascending priority):"
  confds <- loadConfDiffs =<< defaultConfSourcesWith progname confname (Just argsParser)
  print confds
  putStrLn "Resulting Conf:"
  conf <- loadConf =<< defaultConfSourcesWith progname confname (Just argsParser)
  print conf
