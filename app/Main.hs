module Main where

import Protolude
import Fungine.Render.GLFW.WindowSystem
import Fungine.Command
import Fungine.Window
import Fungine.Error
import Fungine

data GameEvent = SetTitle Text
               | QuitGame
               deriving (Show)

data GameState = WindowTitle Text

setTitleE (SetTitle t) = Just $ pure t
setTitleE _            = Nothing

init :: InitF GameEvent GameState
init = (WindowTitle "title", CommandNone)

update :: UpdateF GameEvent GameState
update (SetTitle t) _   = (WindowTitle t, CommandNone)
update QuitGame     mdl = (mdl, CommandExit)

view :: ViewF GameEvent GameState
view = \(WindowTitle t) -> window
  "mainWindow"
  (Fullscreen BestVideoMode)
  t
  [onCreate (SetTitle "created"), onClose QuitGame]


main :: IO ()
main = do
  wstate <- glfwInit
  case wstate of
    Error   t       -> putStrLn t
    Success wstate' -> runFungine glfwPoller wstate' glfwWindowSystem init view update




