{-# LANGUAGE CPP, TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies #-}
module Data.Yaml.Conf where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.Maybe (fromMaybe)
import Data.Monoid
import Control.Monad
import Data.Default

import System.Environment.XDG.BaseDir (getAllConfigFiles)

import Options.Applicative (execParser,ParserInfo)

import Data.Yaml (FromJSON, decodeFileEither,)

import Data.Aeson.TH
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

-- marks data type as config container
class (Default a) => Conf a where

-- marks a generated config diff type
class ConfDiff a where

-- Monoid helpers for instance generation
-- TODO: can this be unified with Generics?
----
-- used for atoms (not-ConfDiff types) -> right Just value wins
updA :: Maybe a -> Maybe a -> Maybe a
updA a b = maybe a Just b
-- used for containers (ConfDiff) -> if both Just, combine diff recursively
updC :: (ConfDiff a, Monoid a) => Maybe a -> Maybe a -> Maybe a
updC (Just a) (Just b) = Just $ a <> b
updC a b = getLast $ Last a <> Last b

-- convert between conf and and its diff, toDiff creates a MINIMAL diff
-- fromDiff creates a config by setting default values at unset fields
-- each conf is part of exactly one such pair. Eq b is auto-derived,
-- Conf / ConfDiff tags are generated, Monoid b is generated respecting Conf(Diff) tags
class (Conf a, Default a, Eq b, Monoid b, ConfDiff b) =>
      ConfDiffRel a b | a->b, b->a where
  toDiff   :: a -> b
  fromDiff :: b -> a
  fromDiff = updateConf def
  updateConf :: a -> b -> a
  updateConf c d = fromDiff (toDiff c <> d)

-- relationship between config and configdiff types
-- described roughly by following commutation diagram:
-- 1 --def---> C
--  \  toDiff | ^
--   def      v | fromDiff
--    \----->  D ⟲ mempty
--             ↻ mappend

-- ConfDiffRel helpers for instance generation
----
-- if equal to default, then nothing. else just value
nonDef :: (Conf a, Eq b) => (a -> b) -> b -> Maybe b
nonDef f v = if f def /= v then Just v else Nothing
-- if missing, take default
maybeDef :: (Conf a) => (a -> b) -> Maybe b -> b
maybeDef f v = fromMaybe (f def) v

-- same for nested config types
nonDef' :: (ConfDiffRel a b) => a -> Maybe b
nonDef' c = if (toDiff c) /= (toDiff def) then Just (toDiff c) else Nothing
maybeDef' :: (ConfDiffRel a b) => Maybe b -> a
maybeDef' = fromDiff . fromMaybe mempty

-- conf management
----
-- load conf diff from args, return difference from default values
getConfFromArgs :: (ConfDiffRel a b) => ParserInfo a -> IO b
getConfFromArgs pinfo = toDiff <$> execParser pinfo

-- try to load file. if it fails, return empty config. otherwise return conf diff
-- TODO: how to handle Left's?
getConfFromFile :: (FromJSON b, ConfDiffRel a b) => FilePath -> IO b
getConfFromFile filename = either (const mempty) id <$> decodeFileEither filename

-- a config is read either from args or from file
-- TODO: maybe also parse from Stringy type?
data ConfSource a b = ArgsConf (ParserInfo a) | FileConf FilePath

-- unified method to get config from args or files
getConf :: (FromJSON b, ConfDiffRel a b) => ConfSource a b -> IO b
getConf (ArgsConf p) = getConfFromArgs p
getConf (FileConf f) = getConfFromFile f

-- returns default XDG directories for configs and a filename for the current dir
defaultConfSources :: String -> String -> IO [FilePath]
defaultConfSources progname confname = (localconf:) <$> getAllConfigFiles progname conffile
  where (conffile, localconf) = (confname++".yaml", progname++'.':conffile)

-- given programname and configname and argparser,
-- return ordered list of sources with increasing priority:
-- system, user, workdir, args
defaultConfSourcesWith :: String -> String -> Maybe (ParserInfo a) -> IO [ConfSource a b]
defaultConfSourcesWith pn cn p = do
  fs <- map FileConf <$> defaultConfSources pn cn
  let srcs = case p of
              Nothing -> fs
              Just pi -> ArgsConf pi : fs
  return $ reverse srcs

-- for debugging. we do not expose diffs to the user
loadConfDiffs :: (FromJSON b, ConfDiffRel a b) => [ConfSource a b] -> IO [b]
loadConfDiffs = mapM getConf

-- given a list of sources, load the configurations, merge and return accumulated
loadConf :: (FromJSON b, ConfDiffRel a b) => [ConfSource a b] -> IO a
loadConf = return . fromDiff . foldl (<>) mempty <=< loadConfDiffs

-- TH magic
----

-- sequence of strings x0, x1, ...
varseq x = map ((x ++).show) [0..]
-- modify names
consName s n = mkName (s ++ nameBase n)
snocName n s = mkName (nameBase n ++ s)
diffName = flip snocName "Diff"
-- returns whether Type is known to have a Conf instance
isConfInstance :: Type -> Q Bool
isConfInstance = isInstance ''Conf . (:[])

-- generates: instance Foo Bar where
mkDummyInstance :: Name -> Name -> DecsQ
mkDummyInstance tc t = mkInstance (AppT (ConT tc) (ConT t)) []

mkInstance :: Type -> [Dec] -> DecsQ
#if MIN_VERSION_template_haskell(2,11,0)
mkInstance t ds = return [InstanceD Nothing [] t ds]
#else
mkInstance t ds = return [InstanceD [] t ds]
#endif

-- is this a single-constructor record type without type variables?
isEligible :: Name -> Q Bool
isEligible tn = do
  info <- reifyDatatype tn
  let cs = datatypeCons info
  if (not $ null $ datatypeVars info) || length cs /= 1 then return False
  else case constructorVariant (head cs) of
         (RecordConstructor _) -> return True
         _ -> return False

-- given a configuration record type, return unwrapped field types
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

-- generate a ConfDiff-type from a Conf data type
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

-- example:
-- instance ConfDiffRel TestConf TestConfDiff where
--   toDiff (TestConf a b) = TestConfDiff (nonDef _foo a) (nonDef _bar b)
--   fromDiff (TestConfDiff a b) = TestConf (maybeDef _foo a) (maybeDef _bar b)
mkRelInstance :: Name -> Name -> DecsQ
mkRelInstance n nd = do
  cfs <- getFields n
  ccons <- getConstr n
  isconf <- mapM (isConfInstance . snd) cfs
  let dcons = diffName ccons
  let vars = take (length cfs) $ varseq "x"
  let mk f = zipWith (\((n,_),c) v -> if c --  for subconfs: non/maybeDef' var
                                      then (VarE $ snocName f "'")`AppE`(VarE v)
                                            -- for leafs: non/maybeDef _field var
                                      else (VarE f)`AppE`(VarE n)`AppE`(VarE v))
                 (cfs `zip` isconf) (mkName <$> vars)
  let to_dec   = FunD 'toDiff   [Clause [ConP ccons $ map (VarP . mkName) vars]
                                 (NormalB $ foldl (AppE) (ConE dcons) $ mk 'nonDef) []]
  let from_dec = FunD 'fromDiff [Clause [ConP dcons $ map (VarP . mkName) vars]
                                 (NormalB $ foldl (AppE) (ConE ccons) $ mk 'maybeDef) []]
  mkInstance (AppT (AppT (ConT ''ConfDiffRel) (ConT n)) (ConT nd)) [to_dec, from_dec]

-- example:
-- instance Default TestConfDiff where
--   def = TestConfDiff (Just $ _foo def) (Just $ _bar def)
-- mkDiffDefaultInstance :: Name -> Name -> DecsQ
-- mkDiffDefaultInstance n nd = do
--   fs <- getFields n
--   isconf <- mapM (\t -> isInstance ''Conf [t]) $ map snd fs
--   constr <- mkName . (++"Diff") . nameBase <$> getConstr n
--   let def_dec = FunD 'def [Clause [] (NormalB $ foldl (AppE) (ConE constr)
--                  $ map (\((f,_),c) -> AppE (ConE 'Just) $ if c then VarE 'def
--                                  else (AppE (VarE f) $ VarE 'def)) $ zip fs isconf) []]
--   return [InstanceD Nothing [] (AppT (ConT ''Default) (ConT nd)) [def_dec] ]

makeConfig :: Name -> DecsQ
makeConfig tname = do
  elig <- isEligible tname
  if not elig then error $ (nameBase tname) ++ " is not a single-constructor primitive record type"
                                            ++ " and can not be used as a configuration container!"
  else do
    let dtname = diffName tname
    cinst <- mkDummyInstance ''Conf tname
    difftype <- mkDiffType tname
    cdinst <- mkDummyInstance ''ConfDiff dtname
    monoinst <- mkMonoidInstance tname dtname
    rinst <- mkRelInstance tname dtname
    return $ cinst ++ difftype ++ cdinst ++ monoinst ++ rinst

-- need to do this in seperate step or GHC complains
deriveConfigJSON :: Name -> DecsQ
deriveConfigJSON tname = do
  deriveJSON defaultOptions{fieldLabelModifier=tail . tail} $ diffName tname
