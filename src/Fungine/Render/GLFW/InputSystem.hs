module Fungine.Render.GLFW.InputSystem where

import Fungine.Prelude
import Fungine.Input
import qualified Graphics.UI.GLFW as G




mapKey :: G.Key -> Int -> Key
mapKey G.Key'Unknown      code = KeyUnknown code
mapKey G.Key'Space        _    = KeySpace
mapKey G.Key'Apostrophe   _    = KeyApostrophe
mapKey G.Key'Comma        _    = KeyComma
mapKey G.Key'Minus        _    = KeyMinus
mapKey G.Key'Period       _    = KeyPeriod
mapKey G.Key'Slash        _    = KeySlash
mapKey G.Key'0            _    = Key0
mapKey G.Key'1            _    = Key1
mapKey G.Key'2            _    = Key2
mapKey G.Key'3            _    = Key3
mapKey G.Key'4            _    = Key4
mapKey G.Key'5            _    = Key5
mapKey G.Key'6            _    = Key6
mapKey G.Key'7            _    = Key7
mapKey G.Key'8            _    = Key8
mapKey G.Key'9            _    = Key9
mapKey G.Key'Semicolon    _    = KeySemicolon
mapKey G.Key'Equal        _    = KeyEqual
mapKey G.Key'A            _    = KeyA
mapKey G.Key'B            _    = KeyB
mapKey G.Key'C            _    = KeyC
mapKey G.Key'D            _    = KeyD
mapKey G.Key'E            _    = KeyE
mapKey G.Key'F            _    = KeyF
mapKey G.Key'G            _    = KeyG
mapKey G.Key'H            _    = KeyH
mapKey G.Key'I            _    = KeyI
mapKey G.Key'J            _    = KeyJ
mapKey G.Key'K            _    = KeyK
mapKey G.Key'L            _    = KeyL
mapKey G.Key'M            _    = KeyM
mapKey G.Key'N            _    = KeyN
mapKey G.Key'O            _    = KeyO
mapKey G.Key'P            _    = KeyP
mapKey G.Key'Q            _    = KeyQ
mapKey G.Key'R            _    = KeyR
mapKey G.Key'S            _    = KeyS
mapKey G.Key'T            _    = KeyT
mapKey G.Key'U            _    = KeyU
mapKey G.Key'V            _    = KeyV
mapKey G.Key'W            _    = KeyW
mapKey G.Key'X            _    = KeyX
mapKey G.Key'Y            _    = KeyY
mapKey G.Key'Z            _    = KeyZ
mapKey G.Key'LeftBracket  _    = KeyLeftBracket
mapKey G.Key'Backslash    _    = KeyBackslash
mapKey G.Key'RightBracket _    = KeyRightBracket
mapKey G.Key'GraveAccent  _    = KeyGraveAccent
mapKey G.Key'World1       _    = KeyWorld1
mapKey G.Key'World2       _    = KeyWorld2
mapKey G.Key'Escape       _    = KeyEscape
mapKey G.Key'Enter        _    = KeyEnter
mapKey G.Key'Tab          _    = KeyTab
mapKey G.Key'Backspace    _    = KeyBackspace
mapKey G.Key'Insert       _    = KeyInsert
mapKey G.Key'Delete       _    = KeyDelete
mapKey G.Key'Right        _    = KeyRight
mapKey G.Key'Left         _    = KeyLeft
mapKey G.Key'Down         _    = KeyDown
mapKey G.Key'Up           _    = KeyUp
mapKey G.Key'PageUp       _    = KeyPageUp
mapKey G.Key'PageDown     _    = KeyPageDown
mapKey G.Key'Home         _    = KeyHome
mapKey G.Key'End          _    = KeyEnd
mapKey G.Key'CapsLock     _    = KeyCapsLock
mapKey G.Key'ScrollLock   _    = KeyScrollLock
mapKey G.Key'NumLock      _    = KeyNumLock
mapKey G.Key'PrintScreen  _    = KeyPrintScreen
mapKey G.Key'Pause        _    = KeyPause
mapKey G.Key'F1           _    = KeyF1
mapKey G.Key'F2           _    = KeyF2
mapKey G.Key'F3           _    = KeyF3
mapKey G.Key'F4           _    = KeyF4
mapKey G.Key'F5           _    = KeyF5
mapKey G.Key'F6           _    = KeyF6
mapKey G.Key'F7           _    = KeyF7
mapKey G.Key'F8           _    = KeyF8
mapKey G.Key'F9           _    = KeyF9
mapKey G.Key'F10          _    = KeyF10
mapKey G.Key'F11          _    = KeyF11
mapKey G.Key'F12          _    = KeyF12
mapKey G.Key'F13          _    = KeyF13
mapKey G.Key'F14          _    = KeyF14
mapKey G.Key'F15          _    = KeyF15
mapKey G.Key'F16          _    = KeyF16
mapKey G.Key'F17          _    = KeyF17
mapKey G.Key'F18          _    = KeyF18
mapKey G.Key'F19          _    = KeyF19
mapKey G.Key'F20          _    = KeyF20
mapKey G.Key'F21          _    = KeyF21
mapKey G.Key'F22          _    = KeyF22
mapKey G.Key'F23          _    = KeyF23
mapKey G.Key'F24          _    = KeyF24
mapKey G.Key'F25          _    = KeyF25
mapKey G.Key'Pad0         _    = KeyPad0
mapKey G.Key'Pad1         _    = KeyPad1
mapKey G.Key'Pad2         _    = KeyPad2
mapKey G.Key'Pad3         _    = KeyPad3
mapKey G.Key'Pad4         _    = KeyPad4
mapKey G.Key'Pad5         _    = KeyPad5
mapKey G.Key'Pad6         _    = KeyPad6
mapKey G.Key'Pad7         _    = KeyPad7
mapKey G.Key'Pad8         _    = KeyPad8
mapKey G.Key'Pad9         _    = KeyPad9
mapKey G.Key'PadDecimal   _    = KeyPadDecimal
mapKey G.Key'PadDivide    _    = KeyPadDivide
mapKey G.Key'PadMultiply  _    = KeyPadMultiply
mapKey G.Key'PadSubtract  _    = KeyPadSubtract
mapKey G.Key'PadAdd       _    = KeyPadAdd
mapKey G.Key'PadEnter     _    = KeyPadEnter
mapKey G.Key'PadEqual     _    = KeyPadEqual
mapKey _                  code = KeyUnknown code

modKeysToList :: G.ModifierKeys -> [ModKey]
modKeysToList mods =
  let
    modkeysLs =
      [ (ModKeyLeftAlt    , G.modifierKeysAlt mods)
      , (ModKeyLeftShift  , G.modifierKeysShift mods)
      , (ModKeyLeftControl, G.modifierKeysControl mods)
      , (ModKeyLeftSuper  , G.modifierKeysSuper mods)
      ]
  in map fst $ filter snd modkeysLs

glfwInputSystem :: InputSystem G.Window
glfwInputSystem = InputSystem { setInputCallback = glfwSetInputCallback }

glfwSetKeyCallback :: G.Window -> (G.Window -> KeyEvent -> IO ()) -> IO ()
glfwSetKeyCallback win f = G.setKeyCallback
  win
  (Just $ \win' key code st mods ->
    let
      key'    = mapKey key code
      modkeys = modKeysToList mods
      evt     = case st of
        G.KeyState'Pressed   -> KeyDownEvent modkeys key'
        G.KeyState'Released  -> KeyUpEvent modkeys key'
        G.KeyState'Repeating -> KeyRepeatingEvent modkeys key'
    in f win' evt
  )


glfwSetMouseCallback :: G.Window -> (G.Window -> MouseEvent -> IO ()) -> IO ()
glfwSetMouseCallback win f = do
  G.setMouseButtonCallback
    win
    (Just $ \win' mb st mods ->
      let
        modkeys = modKeysToList mods
        button  = case mb of
          G.MouseButton'1 -> MouseButton1
          G.MouseButton'2 -> MouseButton2
          G.MouseButton'3 -> MouseButton3
          G.MouseButton'4 -> MouseButton4
          G.MouseButton'5 -> MouseButton5
          G.MouseButton'6 -> MouseButton6
          G.MouseButton'7 -> MouseButton7
          G.MouseButton'8 -> MouseButton8
        ev = case st of
          G.MouseButtonState'Pressed  -> MouseButtonDownEvent modkeys button
          G.MouseButtonState'Released -> MouseButtonUpEvent modkeys button
      in f win' ev
    )
  G.setCursorPosCallback win (Just $ \win' x y -> f win' (MouseMoveEvent x y))


glfwSetInputCallback :: G.Window -> InputCallback G.Window -> IO ()
glfwSetInputCallback win (InputKeyCallback   f) = glfwSetKeyCallback win f
glfwSetInputCallback win (InputMouseCallback f) = glfwSetMouseCallback win f
