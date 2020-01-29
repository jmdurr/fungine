module Main where

import Protolude
import FRP.Behavior
import Fungine.Render.GLFW.WindowSystem
import Fungine.Window
import Fungine.Error
import Fungine

data GameEvent = SetTitle Text
               | StartEvent

setTitleE (SetTitle t) = Just $ pure t
setTitleE _            = Nothing

view :: Behavior GameEvent (Window GameEvent)
view = window
  "mainWindow"
  (Fullscreen BestVideoMode)
  (pure "title" `updateOn` setTitleE)
  (pure [onCreate (SetTitle "created")])


main :: IO ()
main = do
  wstate <- glfwInit
  case wstate of
    Error   t       -> putStrLn t
    Success wstate' -> runFungine wstate' glfwWindowSystem StartEvent view




