{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Path.Tagged (
  -- * Types
  PathTo (..),
  retagPath,
  Base (..),
  Untag,
  Abs,
  RelTo,
  File,
  Dir,

  -- ** Special Entity Tags
  Unknown,
  Subpath (..),
  EntryOf,
  Entry,
  Parent,
  ParentOf,
  SomeBase (..),
  untagSomeBase,

  -- * Exceptions
  PathException (..),

  -- * QuasiQuotes
  absdir,
  reldir,
  absfile,
  relfile,

  -- * Operations
  (</>),
  stripProperPrefix,
  isProperPrefixOf,
  replaceProperPrefix,
  replaceProperPrefix',
  parent,
  filename,
  dirname,
  addExtension,
  splitExtension,
  fileExtension,
  replaceExtension,
  mapSomeBase,
  prjSomeBase,

  -- * Parsing
  parseAbsDir,
  parseRelDir,
  parseAbsFile,
  parseRelFile,
  parseSomeDir,
  parseSomeFile,

  -- * Conversion
  toFilePath,
  fromAbsDir,
  fromRelDir,
  fromAbsFile,
  fromRelFile,
  fromSomeDir,
  fromSomeFile,

  -- * TemplateHaskell constructors
  mkAbsDir,
  mkRelDir,
  mkAbsFile,
  mkRelFile,

  -- ** Typed constructors
  mkAbsDirT,
  mkRelDirT,
  mkAbsFileT,
  mkRelFileT,
) where

import Control.DeepSeq (NFData)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, Value)
import qualified Data.Aeson.Types as J
import qualified Data.Bifunctor as Bi
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Maybe (isJust)
import GHC.Generics (Generic)
import GHC.TypeLits
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift)
import Language.Haskell.TH.Syntax.Compat
import Path (Dir, File, PathException)
import qualified Path as P

type role PathTo phantom nominal nominal

type PathTo :: forall {k}. k -> Base k -> Type -> Type
newtype PathTo entity pk t = PathTo {untagPath :: P.Path (Untag pk) t}
  deriving (Lift, Generic)
  deriving newtype (Eq, Ord, Show, Hashable, ToJSON, NFData)

-- | Unknown base directory
type Unknown :: k
type family Unknown where

type EntryOf :: forall {k}. Subpath k -> k
type family EntryOf k where
  EntryOf ('Entry e) = e
  EntryOf p = TypeError ('Text "Concrete entry expected, but got: " ':<>: 'ShowType p)

data Subpath k = Entry k | Parent (Subpath k)

type Entry = 'Entry

type Parent = 'Parent

type ParentOf e = 'Parent ('Entry e)

retagPath :: PathTo p b t -> PathTo p' b t
retagPath = coerce

deriving newtype instance FromJSON (PathTo k (RelTo b) File)

deriving newtype instance FromJSON (PathTo k (RelTo b) Dir)

deriving newtype instance FromJSON (PathTo k Abs File)

deriving newtype instance FromJSON (PathTo k Abs Dir)

deriving newtype instance FromJSONKey (PathTo k (RelTo b) File)

deriving newtype instance FromJSONKey (PathTo k (RelTo b) Dir)

deriving newtype instance FromJSONKey (PathTo k Abs File)

deriving newtype instance FromJSONKey (PathTo k Abs Dir)

type Untag :: forall {k}. Base k -> Type
type family Untag pk where
  Untag ('RelTo _) = P.Rel
  Untag 'Abs = P.Abs

data Base k = RelTo k | Abs

type RelTo = 'RelTo

type Abs = 'Abs

(</>) :: PathTo parent b Dir -> PathTo child (RelTo parent) t -> PathTo child b t
{-# INLINE (</>) #-}
(</>) = coerce (P.</>)

infixr 5 </>

stripProperPrefix :: MonadThrow m => PathTo p b Dir -> PathTo l b t -> m (PathTo l (RelTo p) t)
stripProperPrefix (PathTo p) (PathTo l) = PathTo <$> P.stripProperPrefix p l

isProperPrefixOf :: PathTo p b Dir -> PathTo l b t -> Bool
isProperPrefixOf p l = isJust (stripProperPrefix p l)

replaceProperPrefix ::
  MonadThrow m =>
  PathTo parent b Dir ->
  PathTo parent' b' Dir ->
  PathTo child b t ->
  m (PathTo child b' t)
replaceProperPrefix src dst fp = (dst </>) <$> stripProperPrefix (retagPath src) fp

replaceProperPrefix' ::
  MonadThrow m =>
  PathTo parent b Dir ->
  PathTo parent b' Dir ->
  PathTo child b t ->
  m (PathTo child b' t)
replaceProperPrefix' src dst fp = (dst </>) <$> stripProperPrefix src fp

parent :: PathTo e b t -> PathTo (Parent e) b Dir
parent = coerce P.parent

filename :: PathTo e b File -> PathTo (Entry e) (RelTo (ParentOf e)) File
filename = coerce P.filename

dirname :: PathTo e b Dir -> PathTo (Entry e) (RelTo (ParentOf e)) Dir
dirname = coerce P.filename

addExtension :: forall e' m e b. MonadThrow m => String -> PathTo e b File -> m (PathTo e' b File)
addExtension ext = fmap PathTo . P.addExtension ext . coerce

splitExtension ::
  forall e' m e b.
  MonadThrow m =>
  PathTo e b File ->
  m (PathTo e' b File, String)
splitExtension = fmap (Bi.first PathTo) . P.splitExtension . coerce

fileExtension :: MonadThrow m => PathTo e b File -> m String
fileExtension = P.fileExtension . coerce

replaceExtension ::
  forall e' m e b.
  MonadThrow m =>
  String ->
  PathTo e b File ->
  m (PathTo e' b File)
replaceExtension ext = fmap PathTo . P.replaceExtension ext . coerce

mapSomeBase :: (forall c. PathTo e c t -> PathTo e' c t') -> SomeBase e b t -> SomeBase e' b t'
mapSomeBase f = \case
  IsAbs fp -> IsAbs (f fp)
  IsRel fp -> IsRel (f fp)

prjSomeBase :: (forall c. PathTo e c t -> a) -> SomeBase e b t -> a
prjSomeBase f = \case
  IsAbs fp -> f fp
  IsRel fp -> f fp

parseAbsDir :: forall e m. MonadThrow m => FilePath -> m (PathTo e Abs Dir)
parseAbsDir = fmap coerce . P.parseAbsDir

parseRelDir :: forall e b m. MonadThrow m => FilePath -> m (PathTo e (RelTo b) Dir)
parseRelDir = fmap coerce . P.parseRelDir

parseAbsFile :: forall e m. MonadThrow m => FilePath -> m (PathTo e Abs File)
parseAbsFile = fmap coerce . P.parseAbsFile

parseRelFile :: forall e b m. MonadThrow m => FilePath -> m (PathTo e (RelTo b) File)
parseRelFile = fmap coerce . P.parseRelFile

parseSomeDir :: MonadThrow m => FilePath -> m (SomeBase e b Dir)
{-# INLINE parseSomeDir #-}
parseSomeDir fp =
  P.parseSomeDir fp <&> \case
    P.Abs p -> IsAbs $ PathTo p
    P.Rel p -> IsRel $ PathTo p

parseSomeFile :: MonadThrow m => FilePath -> m (SomeBase e b File)
{-# INLINE parseSomeFile #-}
parseSomeFile fp =
  P.parseSomeFile fp <&> \case
    P.Abs p -> IsAbs $ PathTo p
    P.Rel p -> IsRel $ PathTo p

data SomeBase e b t
  = IsAbs (PathTo e Abs t)
  | IsRel (PathTo e (RelTo b) t)
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)

instance FromJSON (SomeBase e b Dir) where
  parseJSON = parseJSONWith parseSomeDir
  {-# INLINE parseJSON #-}

instance FromJSON (SomeBase e b File) where
  parseJSON = parseJSONWith parseSomeFile
  {-# INLINE parseJSON #-}

instance ToJSON (SomeBase e b t) where
  toJSON = J.toJSON . prjSomeBase toFilePath

parseJSONWith ::
  (Show e, FromJSON a) =>
  (a -> Either e b) ->
  Value ->
  J.Parser b
parseJSONWith f x =
  do
    fp <- J.parseJSON x
    case f fp of
      Right p -> return p
      Left e -> fail (show e)
{-# INLINE parseJSONWith #-}

untagSomeBase :: SomeBase e b t -> P.SomeBase t
untagSomeBase (IsAbs (PathTo fp)) = P.Abs fp
untagSomeBase (IsRel (PathTo fp)) = P.Rel fp

wrapE :: (FilePath -> ExpQ) -> FilePath -> ExpQ
wrapE e fp = [|PathTo $(e fp)|]

wrapPathQQ :: QuasiQuoter -> QuasiQuoter
wrapPathQQ qq = qq {quoteExp = wrapE (quoteExp qq)}

relfile :: QuasiQuoter
relfile = wrapPathQQ P.relfile

absfile :: QuasiQuoter
absfile = wrapPathQQ P.absfile

reldir :: QuasiQuoter
reldir = wrapPathQQ P.reldir

absdir :: QuasiQuoter
absdir = wrapPathQQ P.absdir

toFilePath :: PathTo e b t -> FilePath
toFilePath = coerce toFilePath

fromAbsDir :: PathTo e Abs Dir -> FilePath
fromAbsDir = P.fromAbsDir . untagPath

fromRelDir :: PathTo e (RelTo b) Dir -> FilePath
fromRelDir = P.fromRelDir . untagPath

fromAbsFile :: PathTo e Abs File -> FilePath
fromAbsFile = P.fromAbsFile . untagPath

fromRelFile :: PathTo e (RelTo b) File -> FilePath
fromRelFile = P.fromRelFile . untagPath

fromSomeDir :: SomeBase e b Dir -> FilePath
fromSomeDir = prjSomeBase toFilePath

fromSomeFile :: SomeBase e b File -> FilePath
fromSomeFile = prjSomeBase toFilePath

mkAbsDir :: FilePath -> ExpQ
mkAbsDir = wrapE mkAbsDir

mkRelDir :: FilePath -> ExpQ
mkRelDir = wrapE mkRelDir

mkAbsFile :: FilePath -> ExpQ
mkAbsFile = wrapE mkAbsFile

mkRelFile :: FilePath -> ExpQ
mkRelFile = wrapE mkRelFile

mkAbsDirT :: FilePath -> SpliceQ (PathTo e Abs Dir)
mkAbsDirT = unsafeSpliceCoerce . mkAbsDir

mkRelDirT :: FilePath -> SpliceQ (PathTo e (RelTo b) Dir)
mkRelDirT = unsafeSpliceCoerce . mkAbsDir

mkAbsFileT :: FilePath -> SpliceQ (PathTo e Abs File)
mkAbsFileT = unsafeSpliceCoerce . mkAbsFile

mkRelFileT :: FilePath -> SpliceQ (PathTo e (RelTo b) File)
mkRelFileT = unsafeSpliceCoerce . mkAbsFile
