{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module BareBonesHttp.Http.RouteHandler where

import BareBonesHttp.Http.Definitions
import BareBonesHttp.Uri.Definitions (Query)
import Control.Applicative
import Control.Lens
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text as T
import Text.Read (readMaybe)

newtype RouteHandler m = RouteHandler {routeFun :: forall c. Request c -> Maybe (m (Response c))}

instance Semigroup (RouteHandler m) where
  (RouteHandler f1) <> (RouteHandler f2) =
    RouteHandler
      (\r -> f1 r `maybeOrElse` f2 r)
    where
      maybeOrElse :: Maybe a -> Maybe a -> Maybe a
      maybeOrElse (Just a) _ = Just a
      maybeOrElse _ (Just a) = Just a
      maybeOrElse Nothing Nothing = Nothing

routeHandlerOrSimpleNotFound :: Applicative m => RouteHandler m -> forall c. Request c -> m (Response c)
routeHandlerOrSimpleNotFound (RouteHandler fun) r = case fun r of
  Just resp -> resp
  Nothing -> pure $ simpleNotFound r

newtype UriMatcher a = UriMatcher {getMatcher :: State (HttpPath, Maybe Query) (Maybe a)}
  deriving (Functor)

instance Applicative UriMatcher where
  pure a = UriMatcher ((pure . pure) a)

  liftA2 f (UriMatcher ma1) (UriMatcher ma2) = UriMatcher ((liftA2 . liftA2) f ma1 ma2)

runMatcher :: UriMatcher a -> (HttpUri -> Maybe a)
runMatcher (UriMatcher matcher) (HttpUri _ p q) =
  let (r, (HttpPath newP, _)) = runState matcher (p, q)
   in if null newP then r else Nothing

data a :/: b = a :/: b

(/:) :: UriMatcher a -> UriMatcher b -> UriMatcher (a :/: b)
a /: b = liftA2 (:/:) a b

matcherFromFun :: (T.Text -> Maybe a) -> UriMatcher a
matcherFromFun f =
  UriMatcher
    ( state
        ( \(HttpPath p, q) -> case p of
            x : xs -> (f x, (HttpPath xs, q))
            [] -> (Nothing, (HttpPath [], q))
        )
    )

exact :: T.Text -> UriMatcher ()
exact t = matcherFromFun (\t' -> if t == t' then Just () else Nothing)

noSegmentsRemaining :: UriMatcher ()
noSegmentsRemaining =
  UriMatcher
    ( state
        ( \(HttpPath p, q) ->
            if null p then (Just (), (HttpPath [], q)) else (Nothing, (HttpPath p, q))
        )
    )

matchString :: UriMatcher T.Text
matchString = matcherFromFun Just

matchInt :: UriMatcher Int
matchInt = matcherFromFun (readMaybe . T.unpack)

matchDouble :: UriMatcher Double
matchDouble = matcherFromFun (readMaybe . T.unpack)

remaining0 :: UriMatcher [T.Text]
remaining0 = UriMatcher (state (\(HttpPath s, q) -> (Just s, (HttpPath [], q))))

remaining1 :: UriMatcher (T.Text, [T.Text])
remaining1 = UriMatcher (fmap (>>= uncons) (getMatcher remaining0))

match :: [Method] -> UriMatcher a -> (Method -> HttpUri -> Maybe a)
match okMethods matcher method uri =
  if method `elem` okMethods
    then runMatcher matcher uri
    else Nothing

handle :: (Method -> HttpUri -> Maybe a) -> (forall c. Request c -> a -> m (Response c)) -> RouteHandler m
handle matcher handler = RouteHandler (\r -> handler r <$> matcher (_requestMethod r) (_requestUri r))

notFoundRoute :: Applicative m => (forall c. Request c -> Response c) -> RouteHandler m
notFoundRoute make = RouteHandler (Just . pure . make)

simpleNotFound :: Request c -> Response c
simpleNotFound r = Response NotFound Map.empty Map.empty Nothing (_requestCap r)

routes :: Applicative m => (forall c. Request c -> Response c) -> [RouteHandler m] -> RouteHandler m
routes makeNotFound = foldr (<>) (notFoundRoute makeNotFound)
