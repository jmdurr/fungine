module Fungine.Render.GLFW.WindowSystem
  ( glfwInit
  , glfwExit
  , glfwWindowSystem
  , glfwPoller
  , GLFWState
  )
where



import Protolude
import Fungine.Error
import Fungine.Render.Window
import Fungine.Window
import qualified Graphics.UI.GLFW as G
import qualified Data.Set as S
import Data.Text (pack, unpack)



data GLFWCallbacks = GLFWCallbacks { fbCb :: Maybe G.FramebufferSizeCallback
                                   , icCb :: Maybe G.WindowIconifyCallback
                                   , focCb :: Maybe G.WindowFocusCallback
                                   , refCb :: Maybe G.WindowRefreshCallback
                                   , closeCb :: Maybe G.WindowCloseCallback
                                   , sizeCb :: Maybe G.WindowSizeCallback
                                   , posCb :: Maybe G.WindowPosCallback
                                   }

glfwPoller :: IO ()
glfwPoller = G.pollEvents

emptyCallbacks :: GLFWCallbacks
emptyCallbacks = GLFWCallbacks Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data GLFWState = GLFWState { primaryMonitor :: Maybe G.Monitor
                           , videoModes :: [G.VideoMode]
                           , bestVideoMode :: Maybe G.VideoMode
                           , fullScreenWindows :: Set G.Window
                           , errors :: MVar Text
                           }

glfwWindowSystem :: WindowSystem GLFWState G.Monitor G.Window e
glfwWindowSystem = WindowSystem
  { resizeWindow         = glfwResizeWindow
  , createWindow         = glfwCreateWindow
  , setWindowTitle       = \w t -> liftIO $ G.setWindowTitle w (unpack t)
  , setCallbacks         = glfwSetCallbacks
  , makeGLContextCurrent = G.makeContextCurrent . Just
  }


glfwInit :: IO (CanError GLFWState)
glfwInit = do
  c <- newEmptyMVar
  G.setErrorCallback $ Just $ \e t -> putMVar c (pack t <> ": " <> show e)
  b <- G.init
  if b
    then do
      m   <- G.getPrimaryMonitor
      vms <- case m of
        Nothing -> pure []
        Just mn -> do
          v <- G.getVideoModes mn
          pure (fromMaybe [] v)
      return $ Success $ GLFWState
        { primaryMonitor    = m
        , videoModes        = vms
        , bestVideoMode     = glfwBestVideoMode vms
        , fullScreenWindows = S.empty
        , errors            = c
        }
    else Error <$> takeMVar c



glfwExit :: IO ()
glfwExit = G.terminate

glfwBestVideoMode :: [G.VideoMode] -> Maybe G.VideoMode
glfwBestVideoMode = foldr
  (\vm2@(G.VideoMode w2 h2 r2 g2 b2 rr2) c -> case c of
    Nothing -> Just vm2
    Just vm1@(G.VideoMode w1 h1 r1 g1 b1 rr1) ->
      if rr1
          >  rr2
          || (rr1 == rr2 && r1 + b1 + g1 > r2 + b2 + g2)
          || (rr1 == rr2 && r1 + b1 + g1 == r2 + b2 + g2 && w1 * h1 > w2 * h2)
        then Just vm1
        else Just vm2
  )
  Nothing

closestMode :: VideoMode -> [G.VideoMode] -> Maybe G.VideoMode
closestMode _ [] = Nothing
closestMode (VideoMode w h bpp) vms =
  let
    (G.VideoMode _ _ _ _ _ mrr) = maximumBy (comparing (\(G.VideoMode _ _ _ _ _ rr) -> rr)) vms
    vms'                        = filter (\(G.VideoMode _ _ _ _ _ rr) -> rr == mrr) vms
    vms''                       = filter (\(G.VideoMode _ _ r _ _ _) -> r == bpp) vms'
    vmsSorted = sortOn (\(G.VideoMode w' h' _ _ _ _) -> abs (w' * h' - w * h)) vms''
  in case vmsSorted of
    []      -> Nothing
    (v : _) -> Just v
closestMode BestVideoMode vms = glfwBestVideoMode vms

fullscreen :: G.Window -> Maybe G.VideoMode -> StateT GLFWState IO ()
fullscreen win vm = do
  m <- gets primaryMonitor
  case (m, vm) of
    (Just m', Just vm') -> do
      lift $ G.setFullscreen win m' vm'
      modify (\s -> s { fullScreenWindows = S.insert win (fullScreenWindows s) })
    (_, _) -> pure ()



glfwResizeWindow :: G.Window -> WindowSize -> StateT GLFWState IO ()
glfwResizeWindow w (Fullscreen BestVideoMode) = do
  md <- gets bestVideoMode
  fullscreen w md

glfwResizeWindow w (Fullscreen vm) = do
  mds <- gets videoModes
  fullscreen w (closestMode vm mds)


glfwResizeWindow win (SizedWindow x y w h) = do
  fswins <- gets fullScreenWindows
  if S.member win fswins
    then do
      lift $ G.setWindowed win x y w h
      modify (\s -> s { fullScreenWindows = S.delete win (fullScreenWindows s) })
    else lift $ do
      G.setWindowPos win x y
      G.setWindowSize win w h

glfwDefaultHints :: [G.WindowHint]
glfwDefaultHints =
  [ G.WindowHint'AutoIconify False
  , G.WindowHint'Decorated False
  , G.WindowHint'DoubleBuffer True
  , G.WindowHint'Focused True
  , G.WindowHint'Resizable True
  , G.WindowHint'Visible True
  , G.WindowHint'ContextVersionMajor 3
  , G.WindowHint'ContextVersionMinor 0
  , G.WindowHint'ContextCreationAPI G.ContextCreationAPI'Native
  , G.WindowHint'OpenGLProfile G.OpenGLProfile'Any
  ]


-- TODO clean this up
glfwCreateWindow :: Window e -> StateT GLFWState IO (CanError G.Window)
glfwCreateWindow win = do
  vms  <- gets videoModes
  mon  <- gets primaryMonitor
  iwin <- liftIO $ maybe
    (pure Nothing)
    (\(w, h) -> do
      G.defaultWindowHints
      mapM_ G.windowHint glfwDefaultHints
      G.createWindow w h (unpack $ wTitle win) (fs mon) Nothing
    )
    (size (wSize win) vms)
  return $ errorMaybe iwin $ pack ("Could not create window with size: " <> show (wSize win))


 where
  vmToSize (G.VideoMode w h _ _ _ _) = (w, h)
  size (Fullscreen BestVideoMode) modes = vmToSize <$> glfwBestVideoMode modes
  size (Fullscreen vm           ) modes = vmToSize <$> closestMode vm modes
  size (SizedWindow _ _ w h     ) _     = Just (w, h)
  fs mon = case wSize win of
    (Fullscreen _) -> mon
    SizedWindow{}  -> Nothing

newIfTrue
  :: Maybe (G.Window -> Bool -> IO ()) -> (G.Window -> IO ()) -> Maybe (G.Window -> Bool -> IO ())
newIfTrue Nothing   f = Just $ \w b -> if b then f w else pure ()
newIfTrue (Just gf) f = Just $ \w b -> if b then f w else gf w b

newIfFalse
  :: Maybe (G.Window -> Bool -> IO ()) -> (G.Window -> IO ()) -> Maybe (G.Window -> Bool -> IO ())
newIfFalse mf f = (\f' w i -> f' w (not i)) <$> newIfTrue mf f

-- restore and iconify are one callback
setCallback :: GLFWCallbacks -> WindowCallback G.Monitor G.Window -> GLFWCallbacks
setCallback cbs (FramebufferSizeCallback f) = cbs { fbCb = Just $ \win w h -> f win (w, h) }
setCallback cbs (IconifyCallback         f) = cbs { icCb = newIfTrue (icCb cbs) f }
setCallback cbs (RestoreCallback         f) = cbs { icCb = newIfFalse (icCb cbs) f }
setCallback cbs (FocusCallback           f) = cbs { focCb = newIfTrue (focCb cbs) f }
setCallback cbs (BlurCallback            f) = cbs { focCb = newIfFalse (focCb cbs) f }
setCallback cbs (RefreshCallback         f) = cbs { refCb = Just f }
setCallback cbs (CloseCallback           f) = cbs { closeCb = Just f }
setCallback cbs (SizeCallback            f) = cbs { sizeCb = Just $ \w x y -> f w (x, y) }
setCallback cbs (PositionCallback        f) = cbs { posCb = Just (\w x y -> f w (x, y)) }


glfwSetCallbacks :: G.Window -> [WindowCallback G.Monitor G.Window] -> StateT GLFWState IO ()
glfwSetCallbacks win ehs = do
  let ncbs = foldr (flip setCallback) emptyCallbacks ehs
  lift $ do
    G.setFramebufferSizeCallback win (fbCb ncbs)
    G.setWindowIconifyCallback win (icCb ncbs)
    G.setWindowFocusCallback win (focCb ncbs)
    G.setWindowRefreshCallback win (refCb ncbs)
    G.setWindowCloseCallback win (closeCb ncbs)
    G.setWindowSizeCallback win (sizeCb ncbs)
    G.setWindowPosCallback win (posCb ncbs)

