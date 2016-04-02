module Main where

import StartApp.Simple as StartApp
import PedChart exposing (init, update, view)

main =
    StartApp.start { model = init, update = update, view = view }
