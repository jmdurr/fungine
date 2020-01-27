module FRP.Behavior where

import           Protolude

newtype Behavior e a = Behavior { step :: e -> (a,Behavior e a)}

instance Functor (Behavior e) where
    fmap ab ba =
        Behavior $ \e -> let (a, ba') = step ba e in (ab a, fmap ab ba')

instance Applicative (Behavior e) where
    pure a = Behavior $ const (a, pure a)
    fab <*> ba = Behavior $ \e ->
        let (ab, fab') = step fab e
            (a , ba' ) = step ba e
        in  (ab a, fab' <*> ba')

instance Semigroup a => Semigroup (Behavior e a) where
    ba <> ba' = Behavior $ \e ->
        let (a , ban ) = step ba e
            (a', ba'n) = step ba' e
        in  (a <> a', ban <> ba'n)


updateOn :: Behavior e a -> (e -> Maybe (Behavior e a)) -> Behavior e a
updateOn ba eba = Behavior $ \e ->
    let (a, ba') = ba `step` e
    in  case eba e of
            Nothing  -> (a, ba' `updateOn` eba)
            Just nba -> (a, nba `updateOn` eba)
