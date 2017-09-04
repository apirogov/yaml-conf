{-# LANGUAGE CPP, TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies #-}
{-|
Module      : Data.Yaml.Conf
Copyright   : Copyright (c) 2017 Anton Pirogov
License     : MIT
Maintainer  : anton.pirogov@gmail.com

This module defines everything needed to extend a record type with the capability
to be constructed from a sequence of YAML files, i.e. from multiple YAML configuration
files that supply a subset of the field values.

To do this, you just need to provide a 'Default' instance,
then call 'makeConfig' and 'deriveConfigJSON' for your record type.
After that you can use 'loadConf' and 'defaultConfSources' to obtain a configuration.

This meshes well with argument parsing, e.g. you can use a parser built with
optparse-applicative to override the values supplied in the files with the values
given as command line parameters.
For this to work, the parser must behave well, i.e. only the values that were
supplied by the user should differ from the values given in the 'Default'
instance.

See <http://github.com/apirogov/yaml-conf> for a working example.
-}
module Data.Yaml.Conf
  (Conf, ConfDiff(..), ConfDiffRel(..), makeConfig, deriveConfigJSON,
  loadConf, mergeConf, defaultConfPaths, defaultConfSources, addConfSource
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid
import Control.Monad
import Data.Default

import System.Environment.XDG.BaseDir (getAllConfigFiles)

import Data.Yaml (FromJSON, decodeFileEither)

import Data.Aeson.TH
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

-- | marks data type as config container. each config has a default value.
class (Default a) => Conf a where

-- Monoid helpers for instance generation
----
-- used for atoms (not-ConfDiff types) -> right Just value wins
updA :: Maybe a -> Maybe a -> Maybe a
updA a b = maybe a Just b
-- used for containers (ConfDiff) -> if both Just, combine diff recursively
updC :: (ConfDiff a) => Maybe a -> Maybe a -> Maybe a
updC (Just a) (Just b) = Just $ a <> b
updC a b = getLast $ Last a <> Last b

-- | marks a generated ConfDiff type, which is a pseudo-torsor on Confs
-- (pseudo because it is just a monoid, not a group).
-- Instances are generated using a derived 'Eq' and a 'Conf'-respecting
-- generated 'Monoid' instance.
class (Eq b, Monoid b) => ConfDiff b where
  -- | 'ConfDiff's can be 'diff'ed (kind-of substraction, but behaves not so well).
  -- This is used to obtain minimal-invasive diffs without redundancy.
  -- Rule: ∀(a,b): a <> ('diff' a b) = b.
  diff :: b -> b -> b

-- | Marks a pair of a configuration and its monoidal diff type.
-- minimal set: 'mkDiff' and ('fromDiff' or 'updateConf')
class (Conf a, ConfDiff b) => ConfDiffRel a b | a->b, b->a where
  -- | Injects all fields of a 'Conf' into a 'ConfDiff' that updates all fields.
  mkDiff   :: a -> b
  -- | Obtain a 'ConfDiff' that patches the first 'Conf' to the second.
  diffConfFrom :: a -> a -> b
  diffConfFrom c c' = (mkDiff c) `diff` (mkDiff c')

  -- | Apply a 'ConfDiff' to a 'Conf'.
  updateConf :: a -> b -> a
  updateConf c d = fromDiff (toDiff c <> d)

  -- these functions work relative to the default configuration:

  -- | Convert a 'Conf' into a 'ConfDiff' (only the changes relative to 'Default').
  toDiff   :: a -> b
  toDiff = diffConfFrom def

  -- | Convert a 'ConfDiff' into a 'Conf', filling in missing values from 'Default'.
  fromDiff :: b -> a
  fromDiff = updateConf def

-- ConfDiffRel helpers for instance generation
----
-- diff helper for regular fields
diffField :: (Eq a) => Maybe a -> Maybe a -> Maybe a
diffField a b = if isNothing b || a==b then Nothing
                else getLast $ Last a <> Last b

-- diff helper for subconfs
diffField' :: (Eq a, ConfDiff a) => Maybe a -> Maybe a -> Maybe a
diffField' a b = case b of
                   Nothing -> Nothing
                   Just w -> case a of
                     Nothing -> Just w
                     Just v -> let u = diff v w
                               in if u==mempty then Nothing else Just u

-- fromDiff helpers:
-- if missing, take default
maybeDef :: (Conf a) => (a -> b) -> Maybe b -> b
maybeDef f v = fromMaybe (f def) v
-- special case when applied to subconfs
maybeDef' :: (ConfDiffRel a b) => Maybe b -> a
maybeDef' = fromDiff . fromMaybe mempty

-- conf management
----

-- try to load file. if it fails, return empty config. otherwise return conf diff
-- TODO: how to handle Left's sanely?
getYamlConf :: (FromJSON b, ConfDiffRel a b) => FilePath -> IO b
getYamlConf filename = either (const mempty) id <$> decodeFileEither filename

-- a config is read either from args or from file

-- | returns default XDG directories for configs and a filename for the current dir
defaultConfPaths :: String -> String -> IO [FilePath]
defaultConfPaths appname cnfname = (localconf:) <$> getAllConfigFiles appname cnffile
  where (cnffile, localconf) = (cnfname++".yaml", appname++'.':cnffile)

-- given programname and configname,
-- return ordered list of sources with increasing priority:
-- | system-wide, user-specific, working directory
defaultConfSources :: (FromJSON b, ConfDiffRel a b) => String -> String -> IO [IO b]
defaultConfSources pn cn = reverse . map getYamlConf <$> defaultConfPaths pn cn

-- | add an additional source (IO action that reads a configuration)
addConfSource :: (ConfDiffRel a b) => IO a -> [IO b] -> [IO b]
addConfSource x xs = xs ++ [toDiff <$> x]

-- | merge a list of configuration diffs
mergeConf :: (ConfDiffRel a b) => [b] -> a
mergeConf = fromDiff . foldl (<>) mempty

-- | given a list of source actions, get the configuration diffs,
-- merge and return accumulated, with missing fields filled up from the default values
loadConf :: (FromJSON b, ConfDiffRel a b) => [IO b] -> IO a
loadConf = return . mergeConf <=< sequence

-- TH magic
----

-- | sequence of strings, for x returns x0, x1, ...
varseq x = map ((x ++).show) [0..]
-- | prepend to a name
consName s n = mkName (s ++ nameBase n)
-- | append to a name
snocName n s = mkName (nameBase n ++ s)

diffName = flip snocName "Diff"

-- | returns whether Type is known to have a Conf instance
isConfInstance :: Type -> Q Bool
isConfInstance = isInstance ''Conf . (:[])

-- | generates: instance Foo Bar where
mkDummyInstance :: Name -> Name -> DecsQ
mkDummyInstance tc t = mkInstance (AppT (ConT tc) (ConT t)) []

-- | version-agnostic wrapper to generate an instance
mkInstance :: Type -> [Dec] -> DecsQ
#if MIN_VERSION_template_haskell(2,11,0)
mkInstance t ds = return [InstanceD Nothing [] t ds]
#else
mkInstance t ds = return [InstanceD [] t ds]
#endif

-- | check that given name is a single-constructor record type without type variables
isEligible :: Name -> Q Bool
isEligible tn = do
  info <- reifyDatatype tn
  let cs = datatypeCons info
  if (not $ null $ datatypeVars info) || length cs /= 1 then return False
  else case constructorVariant (head cs) of
         (RecordConstructor _) -> return True
         _ -> return False

-- | given a configuration record type, return unwrapped field types
getFields :: Name -> Q [(Name,Type)]
getFields d = do
  info <- reifyDatatype d
  let c = head $ datatypeCons info
  let (RecordConstructor ns) = constructorVariant c
  let ts = constructorFields c
  return $ zip ns ts

getConstr :: Name -> Q Name
getConstr d = do
  info <- reifyDatatype d
  return $ constructorName $ head $ datatypeCons info

-- | generate a ConfDiff-type from a Conf data type
mkDiffType :: Name -> DecsQ
mkDiffType tn = do
  fs <- getFields tn
  constr <- getConstr tn
  isconf <- mapM (isConfInstance . snd) fs
  let -- non-conf types need to be wrapped
      modType False t = AppT (ConT ''Maybe) t
      -- conf types also need to be transparently replaced
      modType True (ConT n)  = AppT (ConT ''Maybe) (ConT $ diffName n)
      modType True _  = error "weird, reported Conf instance, but not supported!"
  -- modified types in the Diff type
  let ts = zipWith modType isconf (snd <$> fs)
  -- names with prepended underscores
  let ns = map (consName "_" . fst) fs
  -- generate datatype
  -- TODO: is this strictness stuff ok?
#if MIN_VERSION_template_haskell(2,12,0)
  let bs = repeat (Bang NoSourceUnpackedness NoSourceStrictness)
  let dcon = RecC (diffName constr) $ zip3 ns bs ts
  return [DataD [] (diffName tn) [] Nothing [dcon] $ DeriveClause Nothing [ConT ''Eq, ConT ''Show]]
#elif MIN_VERSION_template_haskell(2,11,0)
  let bs = repeat (Bang NoSourceUnpackedness NoSourceStrictness)
  let dcon = RecC (diffName constr) $ zip3 ns bs ts
  return [DataD [] (diffName tn) [] Nothing [dcon] [ConT ''Eq, ConT ''Show]]
#else
  let bs = repeat NotStrict
  let dcon = RecC (diffName constr) $ zip3 ns bs ts
  return [DataD [] (diffName tn) [] [dcon] [''Eq, ''Show]]
#endif

-- | generate a monoid instance in such a way that atomic fields are overwritten
-- completely, but fields that themselves are sub-configurations are handled in a
-- granular fashion.
mkMonoidInstance :: Name -> Name -> DecsQ
mkMonoidInstance n nd = do
  fs <- getFields n
  ccons <- getConstr n
  let dcons = diffName ccons
  let mempty_dec = FunD 'mempty [Clause [] (NormalB
                 $ foldl (AppE) (ConE dcons) $ replicate (length fs) (ConE 'Nothing)) []]

  let vars = take (length fs) $ mkName <$> varseq "x"
  let vars' = take (length fs) $ mkName <$> varseq "y"
  isconf <- mapM (isConfInstance . snd) fs
  let updf c = if c then 'updC else 'updA
  let updfs = map updf isconf
  let mappend_dec = FunD 'mappend [Clause [ConP dcons $ map VarP vars ,ConP dcons $ map VarP vars']
                  (NormalB $ foldl (AppE) (ConE dcons)
                  $ zipWith3 (\u a b -> (VarE u)`AppE`(VarE a)`AppE`(VarE b)) updfs vars vars') []]

  mkInstance (AppT (ConT ''Monoid) (ConT nd)) [mempty_dec, mappend_dec]

-- | generates a matching pattern, like (Foo a b c)
mkPat :: Name -> [String] -> Pat
mkPat c vs = ConP c $ map (VarP . mkName) vs

-- | generates an instance for the ConfDiffRel class
mkRelInstance :: Name -> Name -> DecsQ
mkRelInstance n nd = do
  cfs <- getFields n
  ccons <- getConstr n
  isconf <- mapM (isConfInstance . snd) cfs
  let dcons = diffName ccons
  let vars = take (length cfs) $ varseq "x"
  let inj = zipWith (\c v -> if c -- for subconfs: convert and wrap
                             then (ConE 'Just)`AppE`((VarE 'mkDiff)`AppE`(VarE v))
                                  -- for leafs - wrap
                             else (ConE 'Just)`AppE`(VarE v)) isconf (mkName <$> vars)
  let frf = zipWith (\((n,_),c) v -> if c --  for subconfs: non/maybeDef' var
                                     then (VarE 'maybeDef')`AppE`(VarE v)
                                          -- for leafs: non/maybeDef _field var
                                     else (VarE 'maybeDef)`AppE`(VarE n)`AppE`(VarE v))
                 (cfs `zip` isconf) (mkName <$> vars)
  let mk_dec   = FunD 'mkDiff   [Clause [mkPat ccons vars]
                                 (NormalB $ foldl (AppE) (ConE dcons) inj) []]
  let from_dec = FunD 'fromDiff [Clause [mkPat dcons vars]
                                 (NormalB $ foldl (AppE) (ConE ccons) frf) []]
  mkInstance (AppT (AppT (ConT ''ConfDiffRel) (ConT n)) (ConT nd))
    [mk_dec, from_dec]

-- | generates an instance for the ConfDiff class, implementing the diff operation
mkDiffInstance :: Name -> Name -> DecsQ
mkDiffInstance n nd = do
  cfs <- getFields n
  ccons <- getConstr n
  isconf <- mapM (isConfInstance . snd) cfs
  let dcons = diffName ccons
  let vars = take (length cfs) $ varseq "x"
  let vars2 = take (length cfs) $ varseq "y"
  let df = zipWith (\c (v1,v2) -> (if c then VarE 'diffField' else VarE 'diffField )
                                    `AppE`(VarE v1)`AppE`(VarE v2))
                   isconf ((mkName <$> vars) `zip` (mkName <$> vars2))
  let diff_dec = FunD 'diff [Clause [mkPat dcons vars, mkPat dcons vars2]
                                 (NormalB $ foldl (AppE) (ConE dcons) df) []]
  mkInstance (AppT (ConT ''ConfDiff) (ConT nd)) [diff_dec]

-- | generates a default instance for the diff type derived from the default
-- instance of the original type. example:
-- instance Default TestConfDiff where
--   def = mkDiff (def :: TestConf)
mkDiffDefaultInstance :: Name -> Name -> DecsQ
mkDiffDefaultInstance n nd = do
  let def_diff = FunD 'def [Clause [] (NormalB
               $ (VarE 'mkDiff)`AppE`(SigE (VarE 'def) (ConT n))) []]
  mkInstance (AppT (ConT ''Default) (ConT nd)) [def_diff]

-- | Call this to generate all the necessary machinery, after providing
-- a default instance for your type. IMPORTANT:
-- If your configuration has nested configurations, you need to call makeConfig on
-- them FIRST!
makeConfig :: Name -> DecsQ
makeConfig tname = do
  elig <- isEligible tname
  if not elig
  then error $ (nameBase tname) ++ " is not a single-constructor primitive record type"
                                ++ " and can not be used as a configuration container!"
  else do
    let dtname = diffName tname
    cinst <- mkDummyInstance ''Conf tname
    difftype <- mkDiffType tname
    cdinst <- mkDiffInstance tname dtname
    monoinst <- mkMonoidInstance tname dtname
    rinst <- mkRelInstance tname dtname
    ddinst <- mkDiffDefaultInstance tname dtname
    return $ cinst ++ difftype ++ cdinst ++ monoinst ++ rinst ++ ddinst

-- | Generates JSON instance for the auto-generated ConfDiff types that is required for
-- the YAML reading. This needs to be done in seperate step (otherwise GHC complains)
deriveConfigJSON :: Name -> DecsQ
deriveConfigJSON tname = do
  deriveJSON defaultOptions{fieldLabelModifier=tail . tail} $ diffName tname
