module Fungine.Render.Component where

import Fungine.Prelude
import Fungine.Component
import Fungine.Graphics.Geometry as G
import Graphics.Rendering.OpenGL.GL.CoordTrans as GLC hiding (Matrix) 
import Graphics.Rendering.OpenGL hiding (Matrix)
import qualified Data.Map as M
import qualified Data.Vector.Storable as VS
import Codec.Picture
import Fungine.Error
import Numeric.LinearAlgebra
import Foreign.Storable (sizeOf)
import Foreign.Ptr
import Data.Text (pack)

-- TODO error handling
loadImage :: UIInfo -> FilePath -> IO (CanError UIInfo)
loadImage info fp = do
  i <- readImage fp
  case (uiFreeBuffers info, i) of
    (_       , Left e    ) -> pure (Fungine.Error.Error (pack e))
    ((b : bs), Right dyni) -> do
      let (Image w h v) = convertRGBA8 dyni
      textureBinding Texture2D $= Just (TextureObject (fromIntegral b))
      VS.unsafeWith v $ \ptr -> texImage2D
        Texture2D
        NoProxy
        0
        RGBA8
        (TextureSize2D (fromIntegral w) (fromIntegral h))
        0
        (PixelData RGBA UnsignedByte ptr)
      pure $ Success $ info
        { uiLoads       = M.insert
          (UIImage fp)
          (UILoadedImage ((w, h), TextureObject (fromIntegral b)))
          (uiLoads info)
        , uiFreeBuffers = bs
        }
    ([], _) -> pure $ Fungine.Error.Error "out of free buffers for ui texture binding"



loadUITextures :: UIInfo -> UIComponent e -> IO (CanError UIInfo)
loadUITextures info c = case (M.lookup (uiRenderable c) (uiLoads info), uiRenderable c) of
  (Nothing, UINoRender  ) -> go (Success info) (uiChildren c)
  (Nothing, UIText _    ) -> go (Success info) (uiChildren c)
  (Nothing, (UIImage fp)) -> do
    img <- loadImage info fp
    go img (uiChildren c)
  (Just r, re) -> go (Success info { uiLoads = M.insert re r (uiLoads info) }) (uiChildren c)
 where
  go (Fungine.Error.Error t    ) _          = pure $ Fungine.Error.Error t
  go (Success             info') []         = pure $ Success info'
  go (Success             info') (cc : ccs) = do
    info'' <- loadUITextures info' cc
    go info'' ccs

returnUnusedBuffers :: UIInfo -> CanError UIInfo -> IO (CanError UIInfo)
returnUnusedBuffers old (Success new) =
  let mp = M.difference (uiLoads old) (uiLoads new)
  in
    do
      let objs = catMaybes $ M.foldr (\r m -> getObjectName r : m) [] mp
      pure $ Success $ new { uiFreeBuffers = objs ++ uiFreeBuffers new }

 where
  getObjectName (UILoadedImage (_, TextureObject i)) = Just i
  getObjectName _ = Nothing

returnUnusedBuffers _ (Fungine.Error.Error t) = pure $ Fungine.Error.Error t

collectEvents :: CanError UIInfo -> UIComponent e -> [e]
collectEvents _ _ = [] -- TODO actually collect events

render :: UIInfo -> UIComponent e -> IO [e]
render info c = do
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  info'  <- loadUITextures info c
  info'' <- returnUnusedBuffers info info'
  ce <- renderComponent info'' (ident 4) c
  whenError ce putStrLn -- TODO better error handling
  pure $ collectEvents info'' c



-- how do we align with camera frustrum
-- if we draw after view has been set...
renderComponent :: CanError UIInfo -> Matrix Float -> UIComponent e -> IO (CanError ())
renderComponent (Fungine.Error.Error t   ) _ _ = pure $ Fungine.Error.Error t
renderComponent (Success             info) _ c = -- TODO use view matrix
  let rot = G.rotate (uiRotation c) (Point3d 0 0 1) -- TODO use rot
      (w,h) = rectangleDim (uiRectangle c info)
      (x,y) = rectangleTopLeft (uiRectangle c info)
      verts = [x+w,y+h,0,0,0,0,0,1,1,0,0,1 -- x,y,r,g,b,a,uv1,uv2,norm
              ,x+w,y,0,0,0,0,0,1,0,0,0,1
              ,x,y,0,0,0,0,0,0,0,0,0,1
              ,x,y+h,0,0,0,0,0,0,1,0,0,1
              ] :: [Float]
      shapes = [0,1,3,1,2,3] :: [Word32]
      tx = M.lookup (uiRenderable c) (uiLoads info)
    in case tx of
      Just (UILoadedImage (_,tobj)) -> do
        vao <- genObjectName 
        vbo <- genObjectName
        ebo <- genObjectName
        bindVertexArrayObject $= Just vao
        bindBuffer ArrayBuffer $= Just vbo
        VS.unsafeWith (VS.fromList verts) $ \ptr -> bufferData ArrayBuffer $= (fromIntegral (length verts * sizeOf (0::Float)),ptr,DynamicDraw)
        bindBuffer ArrayBuffer $= Just ebo
        VS.unsafeWith (VS.fromList shapes) $ \ptr -> bufferData ArrayBuffer $= (fromIntegral (length shapes * sizeOf (0::Word32)),ptr,DynamicDraw)
        vertexAttribPointer (AttribLocation 1) $= (ToFloat, VertexArrayDescriptor 3 Float (fromIntegral (12 * sizeOf (0::Float))) nullPtr)
        vertexAttribArray (AttribLocation 1) $= Enabled
        vertexAttribPointer (AttribLocation 2) $= (ToFloat, VertexArrayDescriptor 3 Float (fromIntegral (12 * sizeOf (0 :: Float))) (plusPtr nullPtr (3*sizeOf (0::Float))))
        vertexAttribArray (AttribLocation 2) $= Enabled
        vertexAttribPointer (AttribLocation 3) $= (ToFloat, VertexArrayDescriptor 2 Float (fromIntegral (12 * sizeOf (0::Float))) (plusPtr nullPtr (6 * sizeOf (0::Float))))
        vertexAttribArray (AttribLocation 3) $= Enabled
        activeTexture $= TextureUnit 0
        textureBinding Texture2D $= Just tobj
        currentProgram $= Just (uiShader info)
        drawElements Triangles 6 UnsignedInt nullPtr
        pure (Success ())
      _ -> return $ Success ()

      


