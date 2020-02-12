module Main where

import Protolude
import Fungine.Render.GLFW
import Fungine.Command
import Fungine.Window
import Fungine.Error
import Fungine
import Fungine.Component.Image
import Fungine.Component.Layout.Align
import Fungine.Component.Layout.AspectRatio
import Fungine.Component.Container
import Fungine.Graphics.Measure

data GameEvent = SetTitle Text
               | ImageGlow (Float,Float,Float,Float)
               | QuitGame
               deriving (Show)

data GameState = GameState { windowTitle :: Text
                           , imageGlow :: (Float,Float,Float,Float)
}

setTitleE (SetTitle t) = Just $ pure t
setTitleE _            = Nothing

init :: InitF GameEvent GameState
init = (GameState { windowTitle = "title", imageGlow = (0, 0, 0, 0) }, CommandNone)

update :: UpdateF GameEvent GameState
update (SetTitle  t) mdl = (mdl { windowTitle = t }, CommandNone)
update (ImageGlow c) mdl = (mdl { imageGlow = c }, CommandNone)
update QuitGame      mdl = (mdl, CommandExit)

view :: ViewF GameEvent GameState
view = \mdl -> window
  "mainWindow"
  (Fullscreen BestVideoMode)
  (windowTitle mdl)
  [onCreate (SetTitle "created"), onClose QuitGame]
  ( centerVAlign
  $ centerHAlign
  $ container [Width (Px 200), Height (Px 100)]
  $ aspectRatio (8, 4)
  $ image "box.jpg"
  )



main :: IO ()
main = do
  gi <- glfwInit
  case gi of
    Error   t        -> putStrLn t
    Success (gs, fs) -> do
      e <- runFungine fs gs init view update
      whenError e putStrLn




