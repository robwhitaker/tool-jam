{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Control.Concurrent.Chan as Chan
import Data.Default (Default, def)
import Path (Abs, Dir, File, Path, (</>))
import qualified Path
import qualified Path.IO as Dir
import qualified System.FSNotify as FSNotify

data Event
  = Added
  | Removed
  | Modified
  deriving stock (Show, Eq)

data Task = Task deriving stock (Show)

data Project = Project deriving stock (Show)

type TaskPath = Path Abs File

type ProjectPath = Path Abs File

data EventHandlers = EventHandlers
  { onTaskAdded :: TaskPath -> Task -> IO (),
    onTaskRemoved :: TaskPath -> Task -> IO (),
    onTaskModified :: TaskPath -> Task -> Task -> IO (),
    onProjectAdded :: ProjectPath -> Project -> IO (),
    onProjectRemoved :: ProjectPath -> Project -> IO (),
    onProjectModified :: ProjectPath -> Project -> Project -> IO (),
    onRawFileEvent :: Event -> Path Abs File -> IO (),
    onRawDirectoryEvent :: Event -> Path Abs Dir -> IO ()
  }

instance Default EventHandlers where
  def =
    EventHandlers
      { onTaskAdded = \_ _ -> pass,
        onTaskRemoved = \_ _ -> pass,
        onTaskModified = \_ _ _ -> pass,
        onProjectAdded = \_ _ -> pass,
        onProjectRemoved = \_ _ -> pass,
        onProjectModified = \_ _ _ -> pass,
        onRawFileEvent = \_ _ -> pass,
        onRawDirectoryEvent = \_ _ -> pass
      }

debugHandlers :: EventHandlers
debugHandlers =
  EventHandlers
    { onTaskAdded = curry print,
      onTaskRemoved = curry print,
      onTaskModified = \x y z -> print (x, y, z),
      onProjectAdded = curry print,
      onProjectRemoved = curry print,
      onProjectModified = \x y z -> print (x, y, z),
      onRawFileEvent = curry print,
      onRawDirectoryEvent = curry print
    }

data Env = Env
  { envTodosDir :: Path Abs Dir,
    envCachedTodosDir :: Path Abs Dir,
    envEventHandlers :: EventHandlers
  }

main :: IO ()
main = do
  let eventHandlers = debugHandlers
  todosDir <- (</>) <$> Dir.getHomeDir <*> pure $(Path.mkRelDir "todos")
  Dir.createDirIfMissing True todosDir

  cacheDir <- Dir.getXdgDir Dir.XdgCache (Just $(Path.mkRelDir "taskd"))
  Dir.createDirIfMissing True cacheDir

  let cachedTodosDir = cacheDir </> $(Path.mkRelDir "todos")
  whenM (Dir.doesDirExist cachedTodosDir) $ Dir.removeDirRecur cachedTodosDir

  Dir.copyDirRecur todosDir cachedTodosDir

  let env = Env todosDir cachedTodosDir eventHandlers

  let mgrConf = FSNotify.defaultConfig {FSNotify.confDebounce = FSNotify.NoDebounce}

  eventStream <- Chan.newChan
  FSNotify.withManagerConf mgrConf $ \mgr -> do
    _ <- FSNotify.watchTreeChan mgr (Path.fromAbsDir todosDir) (const True) eventStream
    void $
      infinitely $
        Chan.readChan eventStream >>= \ev ->
          runReaderT (handleFSEvent ev) env
  where
    handleFSEvent :: FSNotify.Event -> ReaderT Env IO ()
    handleFSEvent (FSNotify.Added filepath _ isDirectory)
      | isDirectory = liftIO (Path.parseAbsDir filepath) >>= handleDir Added
      | otherwise = liftIO (Path.parseAbsFile filepath) >>= handleFile Added
    handleFSEvent (FSNotify.Modified filepath _ isDirectory)
      | isDirectory = liftIO (Path.parseAbsDir filepath) >>= handleDir Modified
      | otherwise = liftIO (Path.parseAbsFile filepath) >>= handleFile Modified
    handleFSEvent (FSNotify.Removed filepath _ isDirectory)
      | isDirectory = liftIO (Path.parseAbsDir filepath) >>= handleDir Removed
      | otherwise = liftIO (Path.parseAbsFile filepath) >>= handleFile Removed
    handleFSEvent unknown = liftIO $ print unknown

    rebase :: Path Abs Dir -> Path Abs Dir -> Path Abs a -> IO (Path Abs a)
    rebase prefix newPrefix path = do
      noPrefix <- Path.stripProperPrefix prefix path
      pure $ newPrefix </> noPrefix

    withCachedVersion :: Path Abs b -> (Path Abs b -> IO a) -> ReaderT Env IO a
    withCachedVersion path f = do
      env <- ask
      cachedPath <- liftIO $ rebase (envTodosDir env) (envCachedTodosDir env) path
      liftIO (f cachedPath)

    withHandlerFor :: (EventHandlers -> handler) -> (handler -> IO ()) -> ReaderT Env IO ()
    withHandlerFor getHandler f = do
      handlers <- asks envEventHandlers
      liftIO $ f (getHandler handlers)

    handleDir :: Event -> Path Abs Dir -> ReaderT Env IO ()
    handleDir Added dir = do
      withCachedVersion dir $ Dir.ignoringAbsence . Dir.copyDirRecur dir
      withHandlerFor onRawDirectoryEvent $ \h -> h Added dir
    handleDir Modified dir = do
      withHandlerFor onRawDirectoryEvent $ \h -> h Modified dir
    handleDir Removed dir = do
      withCachedVersion dir $ Dir.ignoringAbsence . Dir.removeDirRecur
      withHandlerFor onRawDirectoryEvent $ \h -> h Removed dir

    -- TODO: real parsing
    parseTask :: Path Abs File -> IO Task
    parseTask _ = pure Task

    parseProject :: Path Abs File -> IO Project
    parseProject _ = pure Project

    handleFile :: Event -> Path Abs File -> ReaderT Env IO ()
    handleFile Added file = do
      withCachedVersion file $ Dir.ignoringAbsence . Dir.copyFile file
      let fileName = Path.toFilePath (Path.filename file)
      if
          | fileName == "task.yml" -> do
            newTask <- liftIO $ parseTask file
            withHandlerFor onTaskAdded $ \h -> h file newTask
          | fileName == "project.yml" -> do
            newProject <- liftIO $ parseProject file
            withHandlerFor onProjectAdded $ \h -> h file newProject
          | otherwise -> withHandlerFor onRawFileEvent $ \h -> h Added file
    handleFile Modified file = do
      let fileName = Path.toFilePath (Path.filename file)
      if
          | fileName == "task.yml" -> do
            -- TODO: instances of parseTask should eventually handle failures,
            --       e.g. file not found, badly formatted yaml, etc.
            oldTask <- withCachedVersion file parseTask
            newTask <- liftIO $ parseTask file
            withHandlerFor onTaskModified $ \h -> h file oldTask newTask
          | fileName == "project.yml" -> do
            oldProject <- withCachedVersion file parseProject
            newProject <- liftIO $ parseProject file
            withHandlerFor onProjectModified $ \h -> h file oldProject newProject
          | otherwise -> withHandlerFor onRawFileEvent $ \h -> h Modified file
      withCachedVersion file $ Dir.ignoringAbsence . Dir.copyFile file
    handleFile Removed file = do
      let fileName = Path.toFilePath (Path.filename file)
      if
          | fileName == "task.yml" -> do
            oldTask <- withCachedVersion file parseTask
            withHandlerFor onTaskRemoved $ \h -> h file oldTask
          | fileName == "project.yml" -> do
            oldProject <- withCachedVersion file parseProject
            withHandlerFor onProjectRemoved $ \h -> h file oldProject
          | otherwise -> withHandlerFor onRawFileEvent $ \h -> h Removed file
      withCachedVersion file $ Dir.ignoringAbsence . Dir.removeFile
