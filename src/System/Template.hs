module System.Template where

--Windows専用
import System.FilePath.Windows
import System.Directory
import System.Environment
import System.getFilePath

-- TODO 最小のテンプレートを作成する
makeMinimumTemplate :: FilePath -> String -> IO Bool
makeMinimumTemplate targetDir name  = do
    targetIsExistDir <- doesDirectoryExist targetDir
    let templatePath = targetDir </> name
    tempPathExistFlg <- doesPathExist templatePath
    if targetIsExistDir && not tempPathExistFlg
        then do
            createDirectory templatePath
            createDirectory $ templatePath </> "document"
            writeFile (templatePath </> "document" </> "main.lk") "" 
            -- TODO main.lkの中身を記入
            createDirectory $ templatePath </> "helper"
            createDirectory $ templatePath </> "image"
            writeFile (templatePath </> "project.yaml") ""
            -- 中身はそのprojectの設定オプション
            -- TODO project.yamlの中身を記入
            return True
        else return False


-- TODO defaultテンプレートを作成
makeDefaultTemplate :: FilePath -> IO Bool
makeDefaultTemplate = do
    temlatesDir <- getLuksaTemplatesDir
    case templatesDir of
        Nothing -> return False
        Just tsDir -> do
            makeMinimumTemplateDir tsDir "default"
            tempMDir <- getTemplateDir "default"
            let tempDir = takeMaybeFilePath tempMDir
            -- 必要な事項を書き込む
            return True

-- TODO テンプレートを指定のディレクトリにコピーする
copyTemplateTo :: String -> FilePath -> IO Bool
copyTemplateTo tempTarget targetDir = do
    tempMDir <- getTemplateDir tempTarget
    case tempMDir of
        Nothing -> return False
        Just tempDir -> do
            copyTree tempDir targetDir
            return True


