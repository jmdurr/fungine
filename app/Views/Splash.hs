module Views.Splash where

import Fungine.Component
import Fungine.Prelude
import Fungine.Component.Container
import Fungine.Component.Layout.Align
import Fungine.Component.Layout.AspectRatio
import Fungine.Component.Opacity
import Fungine.Component.Image

splash :: Component e
splash = container [] $ centerVAlign $ centerHAlign $ image "./image.png"






{-
a splash screen
  image to be displayed
  animation image
  play some sound
  -- maybe mouse click skips

{-
  centered
   (image 
    filepath
    (width ) 
    [fadeInAnimation 0.25 <> fadeOutAnimation 0.5, onLoad (loopSound snd)])
-}

-}



