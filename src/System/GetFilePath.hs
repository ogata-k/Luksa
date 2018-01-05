module System.GetFilePath where

--Windows専用
import System.FilePath.Windows
import System.Directory
import System.Environment
import Data.List (foldl')

-- 以下、一般的な取得関数
takeMaybeFilePath :: Maybe FilePath -> String
takeMaybeFilePath path = 
    case path of
        Just f -> f
        Nothing -> ""

getChildDir, getChildFile :: IO (Maybe FilePath)
                             -> String
                             -> IO (Maybe FilePath)
getChildDir getParent targetDir = do
    dir <- getParent
    let filePath = do fmap (</> targetDir) dir
    putStrLn $ "search : " ++ takeMaybeFilePath filePath
    existFlag <- case filePath of
                    Just path -> doesDirectoryExist path
                    Nothing -> return False
    if existFlag
        then do
            putStrLn $ "find : " ++ takeMaybeFilePath filePath
            return filePath
        else do
            putStrLn "can not find"
            return Nothing

getChildFile getParent targetFile = do
    dir <- getParent
    let filePath = do fmap (</> targetFile) dir
    putStrLn $ "search : " ++ takeMaybeFilePath filePath
    existFlag <- case filePath of
                    Just path -> doesFileExist path
                    Nothing -> return False
    if existFlag
        then do
            putStrLn $ "find : " ++ takeMaybeFilePath filePath
            return filePath
        else do
            putStrLn "can not find"
            return Nothing


-- 以下、Luksaの実行ファイルからファイルパスを取得する関数 ----------
-- luksa.exeが存在する親ディレクトリを取得
getLuksaDir :: IO (Maybe FilePath)
getLuksaDir = do
    exePath <- getExecutablePath
    let dir = takeDirectory exePath
    existFlg <- doesDirectoryExist dir
    exeExistFlg <- doesFileExist exePath
    if existFlg && exeExistFlg
        then do
            putStrLn $ "find LuksaDir : " ++ dir
            return $ Just dir
        else do
            putStrLn "can not find LuksaDir"
            return Nothing

-- luksa.exeの親ディレクトリから子供を取得
getLuksaDirChildDir = getChildDir getLuksaDir
getLuksaDirChildFile = getChildFile getLuksaDir
getLuksaConfigDir = getLuksaDirChildDir "LuksaConfig"
getTemplatesDir = getLuksaDirChildDir $ "LuksaConfig" </> "templates"
getTemplateDir template = getLuksaDirChildDir $ "LuksaConfig" </> "templates" </> template


-- 以下、プロジェクトに関するファイルパスを取得する関数
-- 最低限プロジェクトの構成は満たしているプロジェクトを取得
getProjectDir :: IO (Maybe FilePath)
-- 現在のディレクトリを取得してそこがプロジェクトと判定されれなければ親でもう一度探す。
getProjectDir = do
    current <- getCurrentDirectory
    home <- getHomeDir
    getProjectDir' home current
    
    where
    -- ルートディレクトリを取得
    getHomeDir :: IO FilePath
    getHomeDir = do
        here <- getCurrentDirectory
        let home = fst $ splitDrive here
        return home

    -- ルートディレクトリになるまで探し続ける
    getProjectDir' :: FilePath -> FilePath -> IO (Maybe FilePath)
    getProjectDir' home currentDir = do
        putStrLn $ "search directory : " ++ currentDir
        -- Luksaのプロジェクト作成コマンド的にプロジェクトの親ディレクトリはホームディレクトリにならないのでホームになったら打ち切り
        if currentDir == home
            then do
                    putStrLn "can not find project"
                    return Nothing
            else do
                existFlg <- isProject currentDir
                if existFlg
                    then do
                        putStrLn $ "find project : " ++ currentDir
                        return $ Just currentDir
                    else do
                        let parent = takeDirectory currentDir
                        getProjectDir' home parent
    -- document,main.lk,helper,imageがあるかを確認。それが通ればそのディレクトリがプロジェクト
    isProject :: FilePath -> IO Bool
    isProject target = do  -- targetは絶対パス
        children <- listDirectory target
        let existFlg = foldl'(&&) True $ map (\f -> f children) $ map elem ["project.yaml", "document", "helper", "image"]
        mainFileExist <- doesFileExist $ target </> "document" </> "main.lk"
        return $ existFlg && mainFileExist

-- projectの子供を取得する関数
getProjectChildDir = getChildDir getProjectDir 
getProjectChildFile = getChildFile getProjectDir
getDocumentDir = getProjectChildDir "document"
getMainLkFile = getProjectChildFile $ "document" </> "main.lk"
getHelperDir = getProjectChildDir "helper"
getImageDir = getProjectChildDir "image"
getConvertedDir = getProjectChildDir "converted"
getProjectYamlFile = getProjectChildFile "project.yaml"

