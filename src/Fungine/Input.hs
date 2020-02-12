module Fungine.Input where

import Fungine.Prelude

data Key = KeyUnknown Int |
            KeySpace |
            KeyApostrophe |
            KeyComma |
            KeyMinus |
            KeyPeriod |
            KeySlash |
            Key0 |
            Key1 |
            Key2 |
            Key3 |
            Key4 |
            Key5 |
            Key6 |
            Key7 |
            Key8 |
            Key9 |
            KeySemicolon |
            KeyEqual |
            KeyA |
            KeyB |
            KeyC |
            KeyD |
            KeyE |
            KeyF |
            KeyG |
            KeyH |
            KeyI |
            KeyJ |
            KeyK |
            KeyL |
            KeyM |
            KeyN |
            KeyO |
            KeyP |
            KeyQ |
            KeyR |
            KeyS |
            KeyT |
            KeyU |
            KeyV |
            KeyW |
            KeyX |
            KeyY |
            KeyZ |
            KeyLeftBracket |
            KeyBackslash |
            KeyRightBracket |
            KeyGraveAccent |
            KeyWorld1 |
            KeyWorld2 |
            KeyEscape |
            KeyEnter |
            KeyTab |
            KeyBackspace |
            KeyInsert |
            KeyDelete |
            KeyRight |
            KeyLeft |
            KeyDown |
            KeyUp |
            KeyPageUp |
            KeyPageDown |
            KeyHome |
            KeyEnd |
            KeyCapsLock |
            KeyScrollLock |
            KeyNumLock |
            KeyPrintScreen |
            KeyPause |
            KeyF1 |
            KeyF2 |
            KeyF3 |
            KeyF4 |
            KeyF5 |
            KeyF6 |
            KeyF7 |
            KeyF8 |
            KeyF9 |
            KeyF10 |
            KeyF11 |
            KeyF12 |
            KeyF13 |
            KeyF14 |
            KeyF15 |
            KeyF16 |
            KeyF17 |
            KeyF18 |
            KeyF19 |
            KeyF20 |
            KeyF21 |
            KeyF22 |
            KeyF23 |
            KeyF24 |
            KeyF25 |
            KeyPad0 |
            KeyPad1 |
            KeyPad2 |
            KeyPad3 |
            KeyPad4 |
            KeyPad5 |
            KeyPad6 |
            KeyPad7 |
            KeyPad8 |
            KeyPad9 |
            KeyPadDecimal |
            KeyPadDivide |
            KeyPadMultiply |
            KeyPadSubtract |
            KeyPadAdd |
            KeyPadEnter |
            KeyPadEqual |
            KeyMenu
            deriving (Eq,Show)

data ModKey = ModKeyLeftAlt
            | ModKeyRightAlt
            | ModKeyLeftShift
            | ModKeyRightShift
            | ModKeyLeftSuper
            | ModKeyRightSuper
            | ModKeyLeftControl
            | ModKeyRightControl
            deriving (Eq,Show)

data KeyEvent = KeyDownEvent [ModKey] Key
              | KeyUpEvent [ModKey] Key
              | KeyRepeatingEvent [ModKey] Key
              deriving (Eq,Show)

data MouseButton = MouseButton1
                 | MouseButton2
                 | MouseButton3
                 | MouseButton4
                 | MouseButton5
                 | MouseButton6
                 | MouseButton7
                 | MouseButton8
                 deriving (Eq,Show)

data MouseEvent = MouseButtonDownEvent [ModKey] MouseButton
                | MouseButtonUpEvent [ModKey] MouseButton
                | MouseMoveEvent Double Double
                deriving (Eq,Show)

data InputCallback win = InputKeyCallback (win -> KeyEvent -> IO ())
                       | InputMouseCallback (win -> MouseEvent -> IO ())


newtype InputSystem win = InputSystem { setInputCallback :: win -> InputCallback win -> IO () }

