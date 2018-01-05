{-# LANGUAGE DataKinds #-}

module Command.LuksaCmds
(Command.LuksaCmds.init
, make
, rename
, convert
, makeTemp
) where

import Options.Declarative
import Control.Monad.IO.Class (liftIO)
import System.Environment
import System.FilePath.Windows  -- widows専用
import System.Directory
import System.Exit (die)
import System.DirectoryCopy (copyTree)


-- Luksaの設定ファイルなどの初期化用コマンド
init :: Cmd "init for Luksa" ()
init = liftIO $ do
    exeParent <- takeDirectory <$> getExecutablePath
    putStr "Start making LuksaConfig directory for initialization of luksa in "
    putStrLn exeParent

    -- 以下でLuksaConfigファイルが存在するか否かで場合分けして処理をする
    configPath <- return $ exeParent </> "LuksaConfig"
    exePath <- return $ exeParent </> "luksa.exe"
    existConfig <- doesDirectoryExist configPath
    existExe <- doesFileExist exePath
    if (existConfig || not existExe)
    then
        putStrLn "Could not initialize luksa because LuksaConfig might be existing. Probably, you had done the command 'luksa init'.\nIf you want to initialize luksa, you remove LuksaConfig directory."
    else do
        putStrLn "start initializing"
        -- ここから初期化開始
        putStrLn "make LuksaConfig"
        createDirectory configPath
        -- templatesディレクトリを作って中にdefaultテンプレートを作成
        putStrLn "make templates"
        createDirectory $ configPath </> "templates"
        putStrLn "make default template"
        defaultMakeFlg <- makeTemplate' "default" $ configPath </> "templates"
        if defaultMakeFlg 
            then putStrLn "success init"
            else die "could not make template. false init."

-- プロジェクトを作成するコマンド
make :: Arg "NAME" String  -- プロジェクト
    -> Flag "t" '["template"] "STRING" "template project" (Def "default" String)  -- テンプレート
    -> Cmd "make project command" ()
make name templ = liftIO $ do
    project <- fmap (</> get name) getCurrentDirectory
    fromTempl <- (</> "LuksaConfig" </> "templates" </> get templ) <$> takeDirectory <$> getExecutablePath
    putStrLn $ "making " ++ get name ++ " project in \n" ++ project ++ "\nwith " ++ get templ ++ " template in \n" ++ fromTempl

    templateExistFlag <- doesDirectoryExist fromTempl
    projectExistFlag <- doesDirectoryExist project
    if templateExistFlag
        then putStrLn "find template"
        else do
            putStrLn "can't find template"
            die "fail making project"
    if projectExistFlag
        then do 
            putStrLn "project already exist."
            die "fail making project"
        else do
            createDirectory project
            putStrLn "make project directory"

    -- テンプレートをコピーする
    copyTree fromTempl project
    putStrLn "success copy template"

    putStrLn "success making projecct"


-- プロジェクトの名前を変更するコマンド
rename :: Arg "TARGET" String
    -> Cmd "rename this project to target" ()
rename = undefined

-- プロジェクトをlatexが扱える形式に変換するコマンド
convert :: Cmd "convert luksa project to latex project" ()
convert = undefined

-- テンプレートの簡単な下書きを提供するコマンド
makeTemp :: Arg "NAME" String
    -> Cmd "make template for NAME template" ()
makeTemp name = liftIO $ do
    -- 排出先を取得(LuksaConfig/templates)
    templatesPath <- fmap (</> "LuksaConfig" </> "templates") $ takeDirectory <$> getExecutablePath
    templatesExistFlg <- doesDirectoryExist templatesPath
    if templatesExistFlg
        then putStrLn "find template directory"
        else die "could not find template directory"

    -- nameでデフォルトのようなテンプレートを作成
    newTemplFlg <- makeTemplate' (get name) templatesPath
    if newTemplFlg
        then putStrLn "success make template of new template"
        else die "could not make template of new template"

-- 下の関数はエラー処理は行っていないに等しい
makeTemplate' :: String -> FilePath -> IO Bool
makeTemplate' name currentDir = do
    templatePath <- return $ currentDir </> name
    tempPathExistFlg <- doesDirectoryExist templatePath
    if tempPathExistFlg
        then return False
        else do
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
