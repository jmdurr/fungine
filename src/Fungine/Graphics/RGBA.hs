module Fungine.Graphics.RGBA where
import Fungine.Prelude
import Foreign.C.Types

type RGBA = CInt

toRGBA :: RGBA -> (Word8,Word8,Word8,Word8)
toRGBA w32 = 
  let a = w32
      b = a `shiftR` 8
      g = b `shiftR` 8
      r = g `shiftR` 8 in
  (fromIntegral (r .&. 0xFF), fromIntegral (g .&. 0xFF), fromIntegral (b .&. 0xFF), fromIntegral (a .&. 0xFF))

fromRGBA :: (Word8,Word8,Word8,Word8) -> RGBA
fromRGBA (r,g,b,a) =
  let v = 0 :: CInt in
    ((((((v .|. fromIntegral r) `shiftL` 8) .|. fromIntegral g) `shiftL` 8) .|. fromIntegral b) `shiftL` 8) .|. fromIntegral a
    

combineRGBA :: (RGBA, Float) -> (RGBA, Float) -> RGBA
combineRGBA  (rgba, w) (rgba', w') =
    let aa = fromIntegral a * w
        ab = fromIntegral a' * w'
        ar = min 255 $ max 0 $ aa * w + ab * w'
        combine c1 c2 = min 255 (max 0 (round $ (fromIntegral c1 * aa + fromIntegral c2 * ab * (1 - aa))/ar))
    in fromRGBA (combine r r', combine g g', combine b b', round ar)
  where (r,g,b,a) = toRGBA rgba
        (r',g',b',a') = toRGBA rgba'