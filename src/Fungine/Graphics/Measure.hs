module Fungine.Graphics.Measure where
import Fungine.Prelude

data Measure = Px Float
             | Percent Float
             | Elements Float


measureToPx :: Float -> Measure -> Float -> Float
measureToPx _  (Px       f) _   = f
measureToPx _  (Percent  f) dim = f * dim
measureToPx emsz (Elements f) _   = emsz * f
