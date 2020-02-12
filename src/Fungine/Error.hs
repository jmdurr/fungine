module Fungine.Error where
import Protolude

data CanError a = Error Text
                | Success a
                deriving Show

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

whenSuccess :: Applicative f => CanError a -> (a -> f ()) -> f ()
whenSuccess (Error   _) _ = pure ()
whenSuccess (Success a) f = f a

fromSuccess :: a -> CanError a -> a
fromSuccess a (Error   _) = a
fromSuccess _ (Success a) = a


errorMaybe :: Maybe a -> Text -> CanError a
errorMaybe Nothing  t = Error t
errorMaybe (Just a) _ = Success a

whenError :: Applicative m => CanError a -> (Text -> m ()) -> m ()
whenError (Error t) f = f t
whenError _ _ = pure ()