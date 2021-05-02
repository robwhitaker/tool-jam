{-# LANGUAGE TemplateHaskell #-}

module Main ( main ) where

import qualified Path.IO as Dir
import qualified Path
import Path (Path, Abs, Rel, File, Dir, (</>))
import qualified System.FSNotify as FSNotify
import qualified Control.Concurrent.Chan as Chan

main :: IO ()
main = do
    todosDir <- (</>) <$> Dir.getHomeDir <*> pure $(Path.mkRelDir "todos")
    Dir.createDirIfMissing True todosDir

    cacheDir <- Dir.getXdgDir Dir.XdgCache (Just $(Path.mkRelDir "taskd"))
    Dir.createDirIfMissing True cacheDir

    let cachedTodos = cacheDir </> $(Path.mkRelDir "todos")
    whenM (Dir.doesDirExist cachedTodos) $
        Dir.removeDirRecur cachedTodos

    events <- Chan.newChan
    FSNotify.withManager $ \mgr -> do
        _ <- FSNotify.watchTreeChan mgr (Path.fromAbsDir todosDir) (const True) events
        void $ infinitely $ Chan.readChan events >>= print
