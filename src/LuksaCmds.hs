{-# LANGUAGE DataKinds #-}

module LuksaCmds where

import Options.Declarative
import Control.Monad.IO.Class (liftIO)
import System.Environment
import System.FilePath.Windows  -- widows専用
import System.Directory
import System.Exit (die)
import DirectoryCopy (copyTree)


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
        defaultTempl <- return $ configPath </> "templates" </> "default"
        createDirectory defaultTempl
        createDirectory $ defaultTempl </> "document"
        writeFile (defaultTempl </> "document" </> "main.lk") ""  -- 中身なし
        -- TODO main.lkの中身を記入
        createDirectory $ defaultTempl </> "helper"
        createDirectory $ defaultTempl </> "impage"
        writeFile (defaultTempl </> "project.yaml") ""
        -- 中身はそのprojectの設定オプション
        -- TODO project.yamlの中身を記入

    putStrLn "success init"


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
makeTemplate :: Arg "NAME" String
    -> Cmd "make template for NAME template" ()
makeTemplate = undefined

