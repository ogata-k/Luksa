# luksa （まだほとんど何もできてない）
luksaはgitやHaskell StackのようなCLIで、MarkDown風に書かれたプロジェクトをコマンドで作成して、LaTeXでコンパイルできるファイル群に変換するプログラムです。

# luksaで扱えるコマンド(よくわからないならluksaと単に実行してみてください)
- init : luksaを初期化して設定フォルダ（LuksaConfigフォルダ）をluksa.exeがあるフォルダに作成するコマンド
- make  [templateOption] <projectName>: プロジェクトを指定したテンプレートで今いるディレクトリ直下に作成する
- rename <targetName>: 今いるプロジェクトの名前を指定した名前に変更する（作らないかも）
- convert : 今いるプロジェクト内部にconvertedディレクトリを作り、LaTeXが変換可能なフォルダ群を吐き出す
- makeTemp : テンプレートのテンプレートを作成（初期のdefaultテンプレートと同じにする）

(削除するにはプロジェクトをディレクトリごと削除することにする)

# プロジェクトの構造（次はdefaultテンプレートで作成したtestプロジェクト）
- test
    - document : 中に変換するmain.lkとそれ以外の変換しないファイルやフォルダを持つ
        - main.lk : このファイルを中心にコンパイルする
    - helper : スタイルファイル用のフォルダ。文書作成の手助けとなるライブラリやモジュールのような.refファイル（luksa独自）や.styファイル（LaTex参照）を持つ
    - image : 画像用のフォルダ（TODO 画像を使うときはqiitaでリンクを張るように使う）
    - project.yaml : プロジェクトの変換後の情報や変換前の構造等を書き込む。haskell stackで作成したプロジェクトのpackage.yamlのようなもの
    - converted : 変換されたファイル群。convertコマンド前は作成されない（TODO このなかでmain.texをコンパイルすればpdf等が作成可能までにしておく）

# 作られたプロジェクトの分野によって使用するタイプで導入するパッケージ群（project.yamlで追記できる）
- base : 必ず初期で入っている
- network : pdfを表示したサイトでクリックできるリンクなどネットで使える便利なパッケージ群を入れる
- bigdata : データが大量なとき用の長い表等のパッケージ群
- programming : プログラミング用に向いているパッケージ群

などなど

# project.yamlになにを書くか
- （プロジェクトの）タイプ
- platexなどのコンパイル時に必要な情報（要らない可能性も）
- documentclassの情報
- latexの方で入っているパッケージ（指定しなくても動くようにしてもいいかも）
