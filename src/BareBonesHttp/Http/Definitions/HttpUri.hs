{-# LANGUAGE OverloadedStrings #-}

module BareBonesHttp.Http.Definitions.HttpUri
  ( HttpDestination (..),
    readHttpDestination,
    HttpUri (..),
    readHttpUri,
    HttpAuthority (..),
    HttpPath (..),
    Port(..),
    Host(..),
  )
where

import BareBonesHttp.Uri
import qualified BareBonesHttp.Uri.Parsers as UriP
import Control.Applicative (liftA2)
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

showMaybePrefix :: Show a => String -> Maybe a -> String
showMaybePrefix prefix = maybe "" (\a -> prefix ++ show a)

data HttpDestination = HttpDestination HttpPath (Maybe Query)

hostParser :: UriP.Parser HttpAuthority
hostParser = liftA2 HttpAuthority UriP.host (P.optional (P.string ":" *> UriP.port))

destinationParser :: UriP.Parser HttpDestination
destinationParser = liftA2 HttpDestination (HttpPath <$> UriP.pathAbsolute) (P.optional (P.string "?" *> UriP.query))

httpUriParser :: UriP.Parser HttpUri
httpUriParser = liftA2 uriFromDestination hostParser destinationParser

readHttpDestination :: T.Text -> Maybe HttpDestination
readHttpDestination t = case P.runParser destinationParser "" t of
  Right a -> Just a
  Left _ -> Nothing

data HttpUri = HttpUri HttpAuthority HttpPath (Maybe Query)

instance Show HttpUri where
  show (HttpUri authority path query') =
    concat ["http://", show authority, show path, showMaybePrefix "?" query']

uriFromDestination :: HttpAuthority -> HttpDestination -> HttpUri
uriFromDestination a (HttpDestination p q) = HttpUri a p q

readHttpUri :: T.Text -> Maybe HttpUri
readHttpUri t = case P.runParser httpUriParser "" t of
  Right a -> Just a
  Left _ -> Nothing

data HttpAuthority = HttpAuthority Host (Maybe Port) deriving (Eq)

instance Show HttpAuthority where
  show (HttpAuthority host' port') = show host' ++ showMaybePrefix ":" port'

newtype HttpPath = HttpPath [T.Text]

instance Show HttpPath where
  show (HttpPath segments) = "/" ++ T.unpack (T.intercalate "/" segments)
