module System.GetFilePath where

--Windows専用
import System.FilePath.Windows
import System.Directory
import System.Environment

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
getProjectDir :: Maybe FilePath
getProjectDir = undefined

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

