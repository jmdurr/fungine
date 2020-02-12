module Fungine.Render.Shader
  ( loadShaders
  , module S
  )
where
import System.Directory
import Paths_fungine
import Fungine.Prelude
import System.FilePath
import Data.Text (pack)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M

import Graphics.Rendering.OpenGL.GL.Shaders
import qualified Graphics.Rendering.OpenGL.GL.Shaders as S (ShaderType(..), Shader)
import Fungine.Error
import Graphics.Rendering.OpenGL (($=))
import qualified Data.Text as T

processDir :: FilePath -> IO (CanError (Map Text (Map ShaderType Shader)))
processDir fp = do
  ls <- listDirectory fp
  processFiles (map ((fp ++ "/") ++) ls)

loadShader :: FilePath -> Text -> IO (CanError (Text, ShaderType, Shader))
loadShader fp fn =
  let
    (st, sn) = T.break (== '_') (T.reverse fn)
    st'      = case T.reverse st of
      "fragment.glsl" -> FragmentShader
      _               -> VertexShader
    sn' = T.reverse (T.dropWhile (== '_') sn)
  in do
    bs <- BS.readFile fp
    s  <- createShader st'
    shaderSourceBS s $= bs
    compileShader s
    e <- compileStatus s
    if not e then Error . pack <$> shaderInfoLog s else pure $ Success (sn', st', s)


processFiles :: [FilePath] -> IO (CanError (Map Text (Map ShaderType Shader)))
processFiles []       = pure $ Success M.empty
processFiles (f : fs) = do
  d <- doesDirectoryExist f
  if d
    then processDir f
    else if takeExtension f == ".glsl"
      then do
        se <- loadShader f (pack $ takeFileName f)
        mp <- processFiles fs
        let semp = (,) <$> se <*> mp
        pure $ fmap
          (\((nm, st, s), mp') -> M.alter
            (\v -> case v of
              Nothing -> Just (M.fromList [(st, s)])
              Just v' -> Just (M.insert st s v')
            )
            nm
            mp'
          )
          semp
      else processFiles fs

createProgram_ :: Map ShaderType Shader -> IO (CanError Program)
createProgram_ mp = do
  p <- createProgram
  mapM_ (\(_, s) -> attachShader p s) (M.toList mp)
  linkProgram p
  s <- linkStatus p
  if not s then programInfoLog p >>= pure . Error . pack else pure (Success p)
  -- TODO mapM_ (\(_,s) -> deleteShader s) (M.toList mp)

createPrograms :: Map Text (Map ShaderType Shader) -> IO (CanError (Map Text Program))
createPrograms mp = do
  ps <- mapM
    (\(nm, smp) -> do
      p <- createProgram_ smp
      pure $ (nm, ) <$> p
    )
    (M.toList mp)
  pure (M.fromList <$> foldr (\c f -> (:) <$> c <*> f) (Success []) ps)

loadShaders :: IO (CanError (Map Text Program))
loadShaders = do
  ddir <- getDataDir
  ls   <- listDirectory (ddir ++ "/shaders")
  mp   <- processFiles (map ((ddir ++ "/shaders/") ++) ls)
  case mp of
    Error   t   -> releaseShaderCompiler >> pure (Error t)
    Success mp' -> do
      ps <- createPrograms mp'
      releaseShaderCompiler
      pure ps
