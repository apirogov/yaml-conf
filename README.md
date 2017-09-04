# yaml-conf  [![Build Status](https://travis-ci.org/apirogov/yaml-conf.svg)](https://travis-ci.org/apirogov/yaml-conf)
<!---
[![Hackage version](https://img.shields.io/hackage/v/cairo-canvas.svg?style=flat)](https://hackage.haskell.org/package/cairo-canvas)
-->

You are looking for a configuration solution that:

* works with your own record-style datatype?
* does not require much additional work?
* integrates well with argument parsing?
* allows to obtain a configuration merging settings from multiple sources?

Then look no more! Look at this example program:

```haskell
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
---- only four magic lines required per Conf type:
instance Default SubConf where
  def = SubConf 0 "default"
makeConfig ''SubConf
deriveConfigJSON ''SubConf
----


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

-- yaml-conf can take any action that returns a configuration as input...
argsParser :: IO TestConf
argsParser = execParser $ info (parseArgs <**> helper)
             (fullDesc <> progDesc "yaml-conf demo")

-- ... so you can provide for example your optparse-applicative parser,
-- after ensuring that uses the specified default values.
parseArgs :: Parser TestConf
parseArgs = TestConf
  <$> switch (long "foo" <> short 'f' <> help "foo blub")
  <*> (SubConf
        <$> option auto (long "uno" <> short 'u' <> value (_uno def))
        <*> option str  (long "dos" <> short 'd' <> value (_dos def))
      )

main :: IO ()
main = do
  let progname = "yaml-conf-demo"
  let confname = "config"

  putStrLn "Default configuration:"
  print (def::TestConf)

  putStrLn "Default file locations (descending priority):"
  print =<< defaultConfPaths progname confname

  sources <- addConfSource argsParser <$> defaultConfSources progname confname

  putStrLn "Loaded configuration diffs (ascending priority):"
  print =<< sequence sources

  putStrLn "Resulting Conf:"
  conf <- loadConf sources
  print conf
```

The program defines a main configuration record type, TestConf,
and a nested sub-configuration, SubConf. The only thing required from you is to
provide default instances and issue the two Template Haskell incantations. The
rest ist done for you automatically!

Now you can obtain a configuration stitched together from multiple sources,
including multiple locations of files and e.g. an optparse-applicative parser. The
only thing that you need to buy into is the fact that the configuration files
are in YAML-format. A valid configuration for the program above looks like this:

```
foo: true
bar:
  dos: "local override"
```

You can place such a configuration into e.g. `~/.config/yaml-conf-demo/config.yaml`.
Notice, that each field of a record marked as a configuration is optional and can be omitted,
you just need to put values for fields you want to override from lower priority
or the default configuration.

For a better understanding, just run this demo application and observe the output.
