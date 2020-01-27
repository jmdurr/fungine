module Fungine.Error where
import           Protolude

data CanError a = Error Text
                | Success a

instance Functor CanError where
    fmap ab fa = case fa of
        Error   t -> Error t
        Success a -> Success (ab a)

instance Applicative CanError where
    pure = Success
    fab <*> fe = case fab of
        Success fa -> fa <$> fe
        Error   t  -> Error t

instance Monad CanError where
    return = pure
    fa >>= afb = case fa of
        Error   t -> Error t
        Success a -> afb a

isError :: CanError a -> Bool
isError (Error   _) = True
isError (Success _) = False

fromSuccess :: a -> CanError a -> a
fromSuccess a (Error   _) = a
fromSuccess _ (Success a) = a
