module Fungine.Render.Shader (loadShaders, module S) where
import System.Directory
import Paths_fungine
import Fungine.Prelude
import System.FilePath
import Data.Text (pack)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Graphics.Rendering.OpenGL.GL.Shaders
import qualified Graphics.Rendering.OpenGL.GL.Shaders as S (ShaderType(..),Shader)
import Fungine.Error
import Graphics.Rendering.OpenGL (($=))
import qualified Data.Text as T

processDir :: FilePath -> IO (Map Text (Map ShaderType Shader))
processDir fp = do
  ls <- listDirectory fp
  processFiles (map ((fp ++ "/") ++ )ls)

-- TODO handle shader errors
loadShader :: FilePath -> Text -> IO (Text,ShaderType,Shader)
loadShader fp fn =
  let (st,sn) = T.break (== '_') (T.reverse fn)
      st' = case T.reverse st of
                "fragment" -> FragmentShader
                _ -> VertexShader
      sn' = T.reverse (T.dropWhile (== '_') sn)
    in do bs <- BS.readFile fp
          s <- createShader st'
          shaderSourceBS s $= bs
          compileShader s
          pure (sn',st',s)


processFiles :: [FilePath] -> IO (Map Text (Map ShaderType Shader))
processFiles [] = pure M.empty
processFiles (f:fs) = do
  d <- doesDirectoryExist f
  if d
    then processDir f
    else if takeExtension f == ".glsl"
      then do
        (nm,st,s) <- loadShader f (pack $ takeFileName f)
        mp <- processFiles fs
        pure (M.alter (\v -> case v of
                            Nothing -> Just (M.fromList [(st,s)])
                            Just v' -> Just (M.insert st s v')
             ) nm mp)
      else processFiles fs

createProgram_ :: Map ShaderType Shader -> IO Program
createProgram_ mp = do
  p <- createProgram 
  mapM_ (\(_,s) -> attachShader p s) (M.toList mp)
  linkProgram p
  --mapM_ (\(_,s) -> deleteShader s) (M.toList mp)
  pure p

createPrograms :: Map Text (Map ShaderType Shader) -> IO (Map Text Program)
createPrograms mp = do
  ps <- mapM (\(nm,smp) -> 
      do p <- createProgram_ smp
         pure (nm,p)
    ) (M.toList mp)
  pure (M.fromList ps)

loadShaders :: IO (CanError (Map Text Program))
loadShaders = do
  ddir <- getDataDir
  ls <- listDirectory (ddir ++ "/shaders")
  mp <- processFiles (traceShowId (map ((ddir ++ "/shaders/") ++ ) ls))
  ps <- createPrograms mp 
  releaseShaderCompiler
  pure (Success ps)
