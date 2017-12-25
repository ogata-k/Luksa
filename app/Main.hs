module Main where

import Options.Declarative(Group(..), subCmd, run_)
import qualified LuksaCmds as LC

main :: IO ()
main = run_ $
    Group "Luksa is a program that makes Latex easy to handle as Haskell Stack"
    [ subCmd "init" LC.init
    , subCmd "make" LC.make
    , subCmd "rename" LC.rename
    , subCmd "convert" LC.convert
    , subCmd "makeTemplate" LC.makeTemplate
    ]
