{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Path.Tagged.IO (
  -- * Actions on directories
  createDir,
  createDirIfMissing,
  ensureDir,
  removeDir,
  removeDirRecur,
  removePathForcibly,
  renameDir,
  renamePath,
  listDir,
  listDirRel,
  listDirRecur,
  listDirRecurRel,
  copyDirRecur,
  copyDirRecur',

  -- * Walking directory trees
  WalkAction (..),
  walkDir,
  walkDirRel,
  walkDirAccum,
  walkDirAccumRel,

  -- * Current working directory
  getCurrentDir,
  setCurrentDir,
  withCurrentDir,

  -- * Pre-defined directories
  PredefinedDir (..),
  WithPredefined,
  Cwd,
  Home,
  AppUserData,
  UserDocs,
  TempDir,
  getHomeDir,
  getAppUserDataDir,
  getUserDocsDir,
  getTempDir,
  XdgDirectory (..),
  XdgData,
  XdgConfig,
  XdgCache,
  WithXdg,
  KnownXdgDirectory (),
  getXdgBaseDir,
  getXdgDir,
  getXdgDataDirs,
  getXdgConfigDirs,

  -- * Path transformation
  AnyPathTo (..),
  RelPathTo,
  resolveFile,
  resolveFile',
  resolveDir,
  resolveDir',

  -- * Actions on Files
  removeFile,
  renameFile,
  copyFile,
  getFileSize,
  findExecutable,
  findFile,
  findFiles,
  findFilesWith,

  -- * Symbolic links
  createFileLink,
  createDirLink,
  removeDirLink,
  getSymlinkTarget,
  isSymlink,

  -- * Temporary files and directories
  withTempFile,
  withTempDir,
  withSystemTempFile,
  withSystemTempDir,
  openTempFile,
  openBinaryTempFile,
  createTempDir,

  -- * Existence tests
  doesPathExist,
  doesFileExist,
  doesDirExist,
  isLocationOccupied,
  forgivingAbsence,
  ignoringAbsence,

  -- * Permissions
  Permissions (..),
  emptyPermissions,
  getPermissions,
  setPermissions,
  copyPermissions,

  -- * Timestamps
  getAccessTime,
  setAccessTime,
  getModificationTime,
  setModificationTime,
) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.Kind (Constraint, Type)
import Data.Time (UTCTime)
import GHC.Exts (Proxy#, proxy#)
import GHC.Generics (Generic)
import Path.IO (Permissions (..), XdgDirectory (..), emptyPermissions, forgivingAbsence, ignoringAbsence)
import qualified Path.IO as P
import Path.Tagged
import System.IO (Handle)

createDir :: MonadIO m => PathTo e b Dir -> m ()
createDir = P.createDir . coerce

createDirIfMissing :: MonadIO m => Bool -> PathTo e b Dir -> m ()
createDirIfMissing p = P.createDirIfMissing p . coerce

ensureDir :: MonadIO m => PathTo e b Dir -> m ()
ensureDir = P.ensureDir . coerce

removeDir :: MonadIO m => PathTo entity b Dir -> m ()
removeDir = P.removeDir . untagPath

removeDirRecur :: MonadIO m => PathTo entity b Dir -> m ()
removeDirRecur = P.removeDirRecur . untagPath

removePathForcibly :: MonadIO m => PathTo e b t -> m ()
removePathForcibly = P.removePathForcibly . untagPath

renameDir :: MonadIO m => PathTo e b Dir -> PathTo e b' Dir -> m ()
renameDir (PathTo old) (PathTo new) = P.renameDir old new

renamePath :: MonadIO m => PathTo e b t -> PathTo e b' t -> m ()
renamePath (PathTo old) (PathTo new) = P.renamePath old new

listDir ::
  MonadIO m =>
  PathTo e b Dir ->
  m ([PathTo Unknown Abs Dir], [PathTo Unknown Abs File])
listDir = fmap coerce . P.listDir . untagPath

listDirRel ::
  MonadIO m =>
  PathTo e b Dir ->
  m ([PathTo Unknown (RelTo e) Dir], [PathTo Unknown (RelTo e) File])
listDirRel = fmap coerce . P.listDirRel . untagPath

listDirRecur ::
  MonadIO m =>
  PathTo e b Dir ->
  m ([PathTo Unknown Abs Dir], [PathTo Unknown Abs File])
listDirRecur = fmap coerce . P.listDirRecur . untagPath

listDirRecurRel ::
  MonadIO m =>
  PathTo e b Dir ->
  m ([PathTo Unknown (RelTo e) Dir], [PathTo Unknown (RelTo e) File])
listDirRecurRel = fmap coerce . P.listDirRecurRel . untagPath

copyDirRecur ::
  (MonadIO m, MonadCatch m) =>
  PathTo e0 b0 Dir ->
  PathTo e1 b1 Dir ->
  m ()
copyDirRecur (PathTo p) (PathTo l) = P.copyDirRecur p l

copyDirRecur' ::
  (MonadIO m, MonadCatch m) =>
  PathTo e0 b0 Dir ->
  PathTo e1 b1 Dir ->
  m ()
copyDirRecur' (PathTo p) (PathTo l) = P.copyDirRecur' p l

data WalkAction b = WalkFinish | WalkExclude [PathTo Unknown b Dir]
  deriving (Show, Eq, Ord)

toUntaggedAct :: WalkAction b -> P.WalkAction (Untag b)
{-# INLINE toUntaggedAct #-}
toUntaggedAct WalkFinish = P.WalkFinish
toUntaggedAct (WalkExclude excs) = P.WalkExclude $ coerce excs

walkDir ::
  MonadIO m =>
  ( forall dir.
    PathTo dir Abs Dir ->
    [PathTo Unknown Abs Dir] ->
    [PathTo Unknown Abs File] ->
    m (WalkAction Abs)
  ) ->
  PathTo e b Dir ->
  m ()
walkDir f p =
  P.walkDir
    ( \dir subdir subfile -> toUntaggedAct <$> f (PathTo dir) (PathTo <$> subdir) (PathTo <$> subfile)
    )
    (untagPath p)

walkDirRel ::
  MonadIO m =>
  ( forall dir.
    PathTo dir (RelTo e) Dir ->
    [PathTo Unknown (RelTo dir) Dir] ->
    [PathTo Unknown (RelTo dir) File] ->
    m (WalkAction (RelTo dir))
  ) ->
  PathTo e b Dir ->
  m ()
walkDirRel f p =
  P.walkDirRel
    ( \dir subdir subfile -> toUntaggedAct <$> f (PathTo dir) (PathTo <$> subdir) (PathTo <$> subfile)
    )
    (untagPath p)

walkDirAccum ::
  (MonadIO m, Monoid o) =>
  Maybe
    ( forall dir.
      PathTo dir Abs Dir ->
      [PathTo Unknown Abs Dir] ->
      [PathTo Unknown Abs File] ->
      m (WalkAction Abs)
    ) ->
  ( forall dir.
    PathTo dir Abs Dir ->
    [PathTo Unknown Abs Dir] ->
    [PathTo Unknown Abs File] ->
    m o
  ) ->
  PathTo e b Dir ->
  m o
walkDirAccum mf g p =
  P.walkDirAccum
    ( mf <&> \f dir subdir subfile -> toUntaggedAct <$> f (PathTo dir) (coerce subdir) (coerce subfile)
    )
    (\dir subdir subfile -> g (PathTo dir) (coerce subdir) (coerce subfile))
    (untagPath p)

walkDirAccumRel ::
  (MonadIO m, Monoid o) =>
  Maybe
    ( forall dir.
      PathTo dir (RelTo e) Dir ->
      [PathTo Unknown (RelTo dir) Dir] ->
      [PathTo Unknown (RelTo dir) File] ->
      m (WalkAction (RelTo dir))
    ) ->
  ( forall dir.
    PathTo dir (RelTo e) Dir ->
    [PathTo Unknown (RelTo dir) Dir] ->
    [PathTo Unknown (RelTo dir) File] ->
    m o
  ) ->
  PathTo e b Dir ->
  m o
walkDirAccumRel mf g p =
  P.walkDirAccumRel
    ( mf <&> \f dir subdir subfile -> toUntaggedAct <$> f (PathTo dir) (coerce subdir) (coerce subfile)
    )
    (\dir subdir subfile -> g (PathTo dir) (coerce subdir) (coerce subfile))
    (untagPath p)

getCurrentDir :: forall m. MonadIO m => m (PathTo Cwd Abs Dir)
getCurrentDir = PathTo <$> P.getCurrentDir

setCurrentDir :: MonadIO m => PathTo e b Dir -> m ()
setCurrentDir = P.setCurrentDir . coerce

withCurrentDir :: (MonadIO m, MonadMask m) => PathTo e b Dir -> m a -> m a
withCurrentDir = P.withCurrentDir . coerce

type WithPredefined :: PredefinedDir -> k
type family WithPredefined p = w | w -> p where

data PredefinedDir
  = Home
  | AppUserData
  | UserDocs
  | TempDir
  | Cwd
  deriving (Show, Eq, Ord, Generic)

type Cwd = WithPredefined 'Cwd

type Home = WithPredefined 'Home

type AppUserData = WithPredefined 'AppUserData

type UserDocs = WithPredefined 'UserDocs

type TempDir = WithPredefined 'TempDir

getHomeDir :: MonadIO m => m (PathTo Home Abs Dir)
getHomeDir = PathTo <$> P.getHomeDir

getAppUserDataDir :: MonadIO m => String -> m (PathTo AppUserData Abs Dir)
getAppUserDataDir = fmap PathTo . P.getAppUserDataDir

getUserDocsDir :: MonadIO m => m (PathTo UserDocs Abs Dir)
getUserDocsDir = PathTo <$> P.getUserDocsDir

getTempDir :: MonadIO m => m (PathTo TempDir Abs Dir)
getTempDir = PathTo <$> P.getTempDir

type WithXdg :: forall {k}. XdgDirectory -> k
type family WithXdg xdg = p | p -> xdg where

type XdgData = WithXdg 'XdgData

type XdgConfig = WithXdg 'XdgConfig

type XdgCache = WithXdg 'XdgCache

type KnownXdgDirectory :: XdgDirectory -> Constraint
class KnownXdgDirectory xdg where
  xdgDirectory# :: Proxy# xdg -> XdgDirectory

instance KnownXdgDirectory 'XdgData where
  xdgDirectory# _ = XdgData

instance KnownXdgDirectory 'XdgConfig where
  xdgDirectory# _ = XdgConfig

instance KnownXdgDirectory 'XdgCache where
  xdgDirectory# _ = XdgCache

getXdgBaseDir ::
  forall xdg m.
  (KnownXdgDirectory xdg, MonadIO m) =>
  m (PathTo xdg Abs Dir)
getXdgBaseDir = PathTo <$> P.getXdgDir (xdgDirectory# (proxy# :: Proxy# xdg)) Nothing

getXdgDir ::
  forall xdg e m.
  (KnownXdgDirectory xdg, MonadIO m) =>
  PathTo e (RelTo (WithXdg xdg)) Dir ->
  m (PathTo e Abs Dir)
getXdgDir (PathTo p) =
  PathTo <$> P.getXdgDir (xdgDirectory# (proxy# :: Proxy# xdg)) (Just p)

getXdgDataDirs :: MonadIO m => m [PathTo XdgData Abs Dir]
getXdgDataDirs = coerce <$> P.getXdgDirList P.XdgDataDirs

getXdgConfigDirs :: MonadIO m => m [PathTo XdgConfig Abs Dir]
getXdgConfigDirs = coerce <$> P.getXdgDirList P.XdgConfigDirs

type RelPathTo :: forall {k}. k -> Type -> Type
type RelPathTo (e :: k) path = RelPathTo' k e path

class AnyPathTo path where
  type PathTag path :: Type
  type AbsPath path :: Type
  type RelPathTo' k (e :: k) path :: Type
  canonicalizePath :: MonadIO m => path -> m (AbsPath path)
  makeAbsolute :: MonadIO m => path -> m (AbsPath path)
  makeRelative :: MonadThrow m => PathTo (e :: PathTag path) Abs Dir -> path -> m (RelPathTo e path)
  makeRelativeToCurrentDir :: MonadIO m => path -> m (RelPathTo' (PathTag path) Cwd path)

instance AnyPathTo (SomeBase (e :: k) b Dir) where
  type PathTag (SomeBase (e :: k) b Dir) = k
  type AbsPath (SomeBase e b Dir) = PathTo e Abs Dir
  type RelPathTo' k e' (SomeBase e b Dir) = PathTo e (RelTo e') Dir
  canonicalizePath = \case
    IsAbs fp -> canonicalizePath fp
    IsRel fp -> canonicalizePath fp
  makeAbsolute = \case
    IsAbs fp -> makeAbsolute fp
    IsRel fp -> makeAbsolute fp
  makeRelative b = \case
    IsAbs fp -> makeRelative b fp
    IsRel fp -> makeRelative b fp
  makeRelativeToCurrentDir = \case
    IsAbs fp -> makeRelativeToCurrentDir fp
    IsRel fp -> makeRelativeToCurrentDir fp

instance AnyPathTo (SomeBase (e :: k) b File) where
  type PathTag (SomeBase (e :: k) b File) = k
  type AbsPath (SomeBase e b File) = PathTo e Abs File
  type RelPathTo' k e' (SomeBase e b File) = PathTo e (RelTo e') File
  canonicalizePath = \case
    IsAbs fp -> canonicalizePath fp
    IsRel fp -> canonicalizePath fp
  makeAbsolute = \case
    IsAbs fp -> makeAbsolute fp
    IsRel fp -> makeAbsolute fp
  makeRelative b = \case
    IsAbs fp -> makeRelative b fp
    IsRel fp -> makeRelative b fp
  makeRelativeToCurrentDir = \case
    IsAbs fp -> makeRelativeToCurrentDir fp
    IsRel fp -> makeRelativeToCurrentDir fp

instance AnyPathTo (PathTo (e :: k) b File) where
  type PathTag (PathTo (e :: k) b File) = k
  type AbsPath (PathTo e b File) = PathTo e Abs File
  type RelPathTo' k e' (PathTo e b File) = PathTo e (RelTo e') File
  canonicalizePath = fmap PathTo . P.canonicalizePath . untagPath
  makeAbsolute = fmap PathTo . P.makeAbsolute . untagPath
  makeRelative (PathTo p) = fmap PathTo . P.makeRelative p . untagPath
  makeRelativeToCurrentDir = fmap PathTo . P.makeRelativeToCurrentDir . untagPath

instance AnyPathTo (PathTo (e :: k) b Dir) where
  type PathTag (PathTo (e :: k) b Dir) = k
  type AbsPath (PathTo e b Dir) = PathTo e Abs Dir
  type RelPathTo' k e' (PathTo e b Dir) = PathTo e (RelTo e') Dir
  canonicalizePath = fmap PathTo . P.canonicalizePath . untagPath
  makeAbsolute = fmap PathTo . P.makeAbsolute . untagPath
  makeRelative (PathTo p) = fmap PathTo . P.makeRelative p . untagPath
  makeRelativeToCurrentDir = fmap PathTo . P.makeRelativeToCurrentDir . untagPath

resolveFile ::
  forall e e0 m.
  MonadIO m =>
  PathTo e0 Abs Dir ->
  FilePath ->
  m (PathTo e Abs File)
resolveFile = fmap (fmap PathTo) . P.resolveFile . untagPath

resolveFile' :: forall e m. MonadIO m => FilePath -> m (PathTo e Abs File)
resolveFile' = fmap PathTo . P.resolveFile'

resolveDir ::
  forall e e0 m.
  MonadIO m =>
  PathTo e0 Abs Dir ->
  FilePath ->
  m (PathTo e Abs Dir)
resolveDir = fmap (fmap PathTo) . P.resolveDir . untagPath

resolveDir' :: forall e m. MonadIO m => FilePath -> m (PathTo e Abs Dir)
resolveDir' = fmap PathTo . P.resolveDir'

removeFile :: MonadIO m => PathTo e b File -> m ()
removeFile = P.removeFile . untagPath

renameFile :: MonadIO m => PathTo e b0 File -> PathTo e b1 File -> m ()
renameFile (PathTo p) = P.renameFile p . untagPath

copyFile :: MonadIO m => PathTo e0 b0 File -> PathTo e1 b1 File -> m ()
copyFile (PathTo p) = P.copyFile p . untagPath

getFileSize :: MonadIO m => PathTo e b File -> m Integer
getFileSize = P.getFileSize . untagPath

findExecutable :: MonadIO m => PathTo e (RelTo b) File -> m (Maybe (PathTo e Abs File))
findExecutable = fmap coerce . P.findExecutable . untagPath

findFile ::
  MonadIO m =>
  [PathTo dir b Dir] ->
  PathTo e (RelTo dir) File ->
  m (Maybe (PathTo e Abs File))
findFile dirs = fmap coerce . P.findFile (coerce dirs) . untagPath

findFiles ::
  MonadIO m =>
  [PathTo dir b Dir] ->
  PathTo e (RelTo dir) File ->
  m [PathTo e Abs File]
findFiles dirs = fmap coerce . P.findFiles (coerce dirs) . untagPath

findFilesWith ::
  MonadIO m =>
  (PathTo e Abs File -> m Bool) ->
  [PathTo dir b Dir] ->
  PathTo e (RelTo dir) File ->
  m [PathTo e Abs File]
findFilesWith p dirs =
  fmap coerce . P.findFilesWith (p . PathTo) (coerce dirs) . untagPath

createFileLink :: MonadIO m => PathTo e b0 File -> PathTo e b1 File -> m ()
createFileLink (PathTo p) = P.createFileLink p . untagPath

createDirLink :: MonadIO m => PathTo e b0 Dir -> PathTo e b1 Dir -> m ()
createDirLink (PathTo p) = P.createDirLink p . untagPath

removeDirLink :: MonadIO m => PathTo e b0 Dir -> m ()
removeDirLink = P.removeDirLink . untagPath

getSymlinkTarget :: MonadIO m => PathTo e b t -> m FilePath
getSymlinkTarget = P.getSymlinkTarget . untagPath

isSymlink :: MonadIO m => PathTo e b t -> m Bool
isSymlink = P.isSymlink . untagPath

withTempFile ::
  forall e e0 b m a.
  (MonadIO m, MonadMask m) =>
  PathTo e0 b Dir ->
  String ->
  (PathTo e Abs File -> Handle -> m a) ->
  m a
withTempFile (PathTo d) name act = P.withTempFile d name $ act . PathTo

withTempDir ::
  (MonadIO m, MonadMask m) =>
  PathTo e b Dir ->
  String ->
  (PathTo TempDir Abs Dir -> m a) ->
  m a
withTempDir (PathTo d) name act = P.withTempDir d name $ act . PathTo

withSystemTempFile ::
  forall e m a.
  (MonadIO m, MonadMask m) =>
  String ->
  (PathTo e Abs File -> Handle -> m a) ->
  m a
withSystemTempFile name act = P.withSystemTempFile name $ act . PathTo

withSystemTempDir ::
  (MonadIO m, MonadMask m) =>
  String ->
  (PathTo TempDir Abs Dir -> m a) ->
  m a
withSystemTempDir name act = P.withSystemTempDir name $ act . PathTo

openTempFile ::
  forall e e0 b m.
  MonadIO m =>
  PathTo e0 b Dir ->
  String ->
  m (PathTo e Abs File, Handle)
openTempFile (PathTo p) = fmap coerce . P.openTempFile p

openBinaryTempFile ::
  forall e e0 b m.
  MonadIO m =>
  PathTo e0 b Dir ->
  String ->
  m (PathTo e Abs File, Handle)
openBinaryTempFile (PathTo p) = fmap coerce . P.openBinaryTempFile p

createTempDir :: MonadIO m => PathTo e b Dir -> String -> m (PathTo TempDir Abs Dir)
createTempDir (PathTo p) = fmap PathTo . P.createTempDir p

doesPathExist :: MonadIO m => PathTo e b t -> m Bool
doesPathExist = P.doesPathExist . untagPath

doesFileExist :: MonadIO m => PathTo e b File -> m Bool
doesFileExist = P.doesFileExist . untagPath

doesDirExist :: MonadIO m => PathTo e b Dir -> m Bool
doesDirExist = P.doesDirExist . untagPath

isLocationOccupied :: MonadIO m => PathTo e b t -> m Bool
isLocationOccupied = P.isLocationOccupied . untagPath

getPermissions :: MonadIO m => PathTo e b t -> m Permissions
getPermissions = P.getPermissions . untagPath

setPermissions :: MonadIO m => PathTo e b t -> Permissions -> m ()
setPermissions = P.setPermissions . untagPath

copyPermissions :: MonadIO m => PathTo e0 b0 t0 -> PathTo e1 b1 t1 -> m ()
copyPermissions (PathTo p) = P.copyPermissions p . untagPath

getAccessTime :: MonadIO m => PathTo entity pk t -> m UTCTime
getAccessTime = P.getAccessTime . untagPath

setAccessTime :: MonadIO m => PathTo entity pk t -> UTCTime -> m ()
setAccessTime = P.setAccessTime . untagPath

getModificationTime :: MonadIO m => PathTo entity pk t -> m UTCTime
getModificationTime = P.getModificationTime . untagPath

setModificationTime :: MonadIO m => PathTo entity pk t -> UTCTime -> m ()
setModificationTime = P.setModificationTime . untagPath
