module System.GetFilePath where

--Windows専用
import System.FilePath.Windows
import System.Directory
import System.Environment

-- 以下、Luksaの実行ファイルからファイルパスを取得する関数
getLuksaDir :: IO (Maybe FilePath)
getLuksaDir = do
    exePath <- getExecutablePath
    let dir = takeDirectory exePath
    existFlg <- doesDirectoryExist dir
    return $ if existFlg then Just dir else Nothing

getLuksaConfigDir:: IO (Maybe FilePath)
getLuksaConfigDir = do
    dir <- getLuksaDir
    let filePath = do fmap (</> "LuksaConfig") dir
    existFlag <- case filePath of
                    Just path -> doesDirectoryExist path
                    Nothing -> return False
    return $ if existFlag then filePath else Nothing

getTemplatesDir :: Maybe FilePath
getTemplatesDir = undefined

getTemplateDir :: String -> Maybe FilePath
getTemplateDir = undefined

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

