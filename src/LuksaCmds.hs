{-# LANGUAGE DataKinds #-}

module LuksaCmds where

import Options.Declarative


-- Luksaの設定ファイルなどの初期化用コマンド
init :: Cmd "init for Luksa" ()
init = undefined

-- プロジェクトを作成するコマンド
make :: Arg "NAME" String
    -> Flag "t" '["template"] "STRING" "template project" (Def "default" String)
    -> Cmd "make project command" ()
make = undefined

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

