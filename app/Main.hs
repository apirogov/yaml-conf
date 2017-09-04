{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies #-}
module Main where

import Data.Monoid
import Options.Applicative
import Data.Default
import Data.Yaml.Conf

-- | the nested inner configuration record
data SubConf = SubConf {
    _uno :: Int
  , _dos :: String
  } deriving (Show)
---- only four magic lines required per Conf type:
instance Default SubConf where
  def = SubConf 0 "default"
makeConfig ''SubConf
deriveConfigJSON ''SubConf
----

-- | the outer configuration record
data TestConf = TestConf {
    _foo :: Bool
  , _bar :: SubConf
  } deriving (Show)
---- for the main configuration type too:
instance Default TestConf where
  def = TestConf False def
makeConfig ''TestConf
deriveConfigJSON ''TestConf
----

-- | yaml-conf can take any action that returns a configuration as input...
argsParser :: IO TestConf
argsParser = execParser $ info (parseArgs <**> helper)
             (fullDesc <> progDesc "yaml-conf demo")

-- | ... so you can provide for example your optparse-applicative parser,
-- after ensuring that uses the specified default values.
parseArgs :: Parser TestConf
parseArgs = TestConf
  <$> switch (long "foo" <> short 'f' <> help "foo blub")
  <*> (SubConf
        <$> option auto (long "uno" <> short 'u' <> value (_uno def))
        <*> option str  (long "dos" <> short 'd' <> value (_dos def))
      )

-- | just run this to see for yourself:
main :: IO ()
main = do
  let progname = "yaml-conf-demo"
  let confname = "config"

  putStrLn "Default configuration:"
  print (def::TestConf)

  putStrLn "Default file locations (descending priority):"
  print =<< defaultConfPaths progname confname

  putStrLn "Loaded configuration diffs (ascending priority):"
  sources <- addConfSource argsParser <$> defaultConfSources progname confname
  print =<< sequence sources

  putStrLn "Resulting Conf:"
  conf <- loadConf sources
  print conf
