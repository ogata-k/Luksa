module System.GetFilePath where

--Windows専用
import System.FilePath.Windows
import System.Directory
import System.Environment
import Data.List (foldl')

-- MaybeなFilePathから取得
takeMaybeFilePath :: Maybe FilePath -> String
takeMaybeFilePath path = 
    case path of
        Just f -> f
        Nothing -> ""

-- 以下、Luksaの実行ファイルからファイルパスを取得する関数 ----------
-- luksa.exeが存在する親ディレクトリを取得
getLuksaDir :: IO (Maybe FilePath)
getLuksaDir = do
    exePath <- getExecutablePath
    let dir = takeDirectory exePath
    existFlg <- doesDirectoryExist dir
    exeExistFlg <- doesFileExist exePath
    return $ if existFlg && exeExistFlg then Just dir else Nothing
-- luksa.exeの親ディレクトリから子供を取得
getLuksaDirChildDir, getLuksaDirChildFile :: String -> IO (Maybe FilePath)
getLuksaDirChildDir targetDir = do
    dir <- getLuksaDir
    let filePath = do fmap (</> targetDir) dir
    putStrLn $ "search : " ++ takeMaybeFilePath filePath
    existFlag <- case filePath of
                    Just path -> doesDirectoryExist path
                    Nothing -> return False
    return $ if existFlag then filePath else Nothing

getLuksaDirChildFile targetFile = do
    dir <- getLuksaDir
    let filePath = do fmap (</> targetFile) dir
    putStrLn $ "search : " ++ takeMaybeFilePath filePath
    existFlag <- case filePath of
                    Just path -> doesFileExist path
                    Nothing -> return False
    return $ if existFlag then filePath else Nothing
-- それぞれ特殊なファイルようの取得関数
getLuksaConfigDir = getLuksaDirChildDir "LuksaConfig"
getTemplatesDir = getLuksaDirChildDir $ "LuksaConfig" </> "templates"
getTemplateDir template = getLuksaDirChildDir $ "LuksaConfig" </> "templates" </> template

-- 以下、プロジェクトに関するファイルパスを取得する関数
-- 最低限プロジェクトの構成は満たしているプロジェクトを取得
getProjectDir :: IO (Maybe FilePath)
-- 現在のディレクトリを取得してその中にproject.yamlがなければ親でもう一度探す。あればdocument,main.lk,helper,imageがあるかを確認。それが通ればそのディレクトリがプロジェクトとする。
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
            then return Nothing
            else do
                existFlg <- isProject currentDir
                if existFlg
                    then do
                        putStrLn $ "find : " ++ currentDir
                        return $ Just currentDir
                    else do
                        let parent = takeDirectory currentDir
                        getProjectDir' home parent

    isProject :: FilePath -> IO Bool
    isProject target = do  -- targetは絶対パス
        children <- listDirectory target
        let existFlg = foldl'(&&) True $ map (\f -> f children) $ map elem ["project.yaml", "document", "helper", "image"] -- ここは用編集
        mainFileExist <- doesFileExist $ target </> "document" </> "main.lk"
        return $ if existFlg && mainFileExist then True else False

getDocumentDir :: Maybe FilePath
getDocumentDir = undefined

getMainLkFile :: Maybe FilePath
getMainLkFile = undefined

getHelperDir :: Maybe FilePath
getHelperDir = undefined

getImageDir :: Maybe FilePath
getImageDir = undefined

getConvertedDir :: Maybe FilePath
getConvertedDir = undefined

getProjectYamlFile :: Maybe FilePath
getProjectYamlFile = undefined

