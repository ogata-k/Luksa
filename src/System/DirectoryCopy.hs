module System.DirectoryCopy (copyTree) where

import System.Directory
import System.Environment
import System.FilePath
import Data.Functor ((<$>))
import Data.List ((\\))

getSubitems :: FilePath -> IO [(Bool, FilePath)]
getSubitems path = getSubitemsRec ""
  where
    getChildren path =  (\\ [".", ".."]) <$> getDirectoryContents path

    getSubitemsRec relPath = do
        let absPath = path </> relPath
        isDir <- doesDirectoryExist absPath
        children <- if isDir then getChildren absPath else return []
        let relChildren = [relPath </> p | p <- children]
        ((isDir, relPath) :) . concat <$> mapM getSubitemsRec relChildren

copyItem baseSourcePath baseTargetPath (isDir, relativePath) = do
    let sourcePath = baseSourcePath </> relativePath
    let targetPath = baseTargetPath </> relativePath

    putStrLn $ "Copying " ++ sourcePath ++ " to " ++ targetPath
    if isDir
      then createDirectoryIfMissing False targetPath
      else copyFile sourcePath targetPath

copyTree :: FilePath -> FilePath -> IO ()
copyTree s t = do
    createDirectoryIfMissing True t
    subItems <- getSubitems s
    mapM_ (copyItem s t) subItems

