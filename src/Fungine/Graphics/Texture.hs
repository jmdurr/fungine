module Fungine.Graphics.Texture where
import Fungine.Prelude
import Fungine.Graphics.RGBA
import Numeric.LinearAlgebra
data Texture = StaticImage (Matrix RGBA)
