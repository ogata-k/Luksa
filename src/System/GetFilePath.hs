module System.GetFilePath where

import System.FilePath.Windows  --Windows専用
import System.Environment

-- 以下、Luksaの実行ファイルからファイルパスをを取得する関数
getLuksaExeFile :: Maybe FilePath
getLuksaExeFile = undefined

getLuksaConfigDir :: Maybe FilePath
getLuksaConfigDir = undefined

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

