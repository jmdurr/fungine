module Fungine.Render.GLFW where

import Fungine.Render.GLFW.WindowSystem
import Fungine.Render.GLFW.InputSystem
import Fungine
import Fungine.Prelude
import Fungine.Error
import qualified Graphics.UI.GLFW as G

glfwInit :: IO (CanError (GLFWState, FungineSystem GLFWState G.Monitor G.Window e))
glfwInit = do
  b <- G.init
  if b
    then do
      gs <- glfwWindowInit
      pure
        $ Success
        $ ( gs
          , FungineSystem
            { fWindowSystem = glfwWindowSystem
            , fInputSystem  = glfwInputSystem
            , fPollEvents   = G.pollEvents
            }
          )
    else pure $ Error $ "Cannot initialize GLFW"
