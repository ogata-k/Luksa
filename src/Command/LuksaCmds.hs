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
import System.GetFilePath
import System.Template
import Control.Monad (when)

-- Luksaの設定ファイルなどの初期化用コマンド
init :: Cmd "init for Luksa" ()
init = liftIO $ do
    putStr "Start making LuksaConfig directory for initialization of luksa in "
    parent <- getLuksaDir
    when (parent == Nothing) $ do
        putStrLn "can not find directory which has exe file"
        die "fail init"
    let exeParent = takeMaybeFilePath parent
    putStrLn exeParent

    -- 以下でLuksaConfigファイルが存在するか否かで場合分けして処理をする
    let configPath = exeParent </> "LuksaConfig"
    let exePath = exeParent </> "luksa.exe"
    existConfigFlg <- doesDirectoryExist configPath
    existExe <- doesFileExist exePath
    if existConfigFlg
        then do
            putStrLn "Could not initialize luksa because of existing LuksaConfig . Probably, you had done the command 'luksa init'.\nIf you want to initialize luksa, you remove LuksaConfig directory."
            die "fail init"
        else do
            -- ここから初期化開始
            putStrLn "make LuksaConfig"
            createDirectory configPath
            -- templatesディレクトリを作って中にdefaultテンプレートを作成
            putStrLn "make templates"
            createDirectory $ configPath </> "templates"
            putStrLn "make default template"
            defaultMakeFlg <- makeDefaultTemplate
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
    templatesMPath <- getTemplatesDir
    case templatesMPath of
        Nothing -> do
            putStrLn "can not find templates directory"
            die "fail make template"
        Just templatesPath -> do
            -- nameで最低限のテンプレート用テンプレートを作成
            newTemplFlg <- makeMinimumTemplate templatesPath (get name)
            if newTemplFlg
                then putStrLn "success make template of new template"
                else die "could not make template of new template"

