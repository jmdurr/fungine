module Fungine.Component.Opacity where
import Fungine.Prelude
import Fungine.Component

opacity :: Float -> UIComponent e -> UIComponent e
opacity amt c = c{ uiOpacity = uiOpacity c * amt
                 , uiChildren = map (opacity amt) (uiChildren c)
                 }