{-# LANGUAGE TemplateHaskell #-}

module BareBonesHttp.Uri.Definitions
  ( Scheme,
    _schemeContent,
    schemeContent,
    mkScheme,
    UserInfo (..),
    userInfoText,
    Host (..),
    ipLiteralText,
    ipv4Text,
    hostName,
    Port (..),
    portNum,
    Authority (..),
    userInfo,
    host,
    port,
    HierPart (..),
    hierAuthority,
    hierFirst,
    hierSegments,
    RelativePart (..),
    relAuthority,
    relFirst,
    relSegments,
    Path (..),
    pathFirst,
    pathSegments,
    pathOfHierPart,
    pathOfRelPart,
    Query (..),
    queryText,
    Fragment (..),
    fragmentText,
    Uri (..),
    scheme,
    hierPart,
    query,
    fragment,
    UriReference (..),
  )
where

import Control.Lens
import Data.List (intercalate)
import qualified Data.Text as T

showMaybePrefix :: Show a => String -> Maybe a -> String
showMaybePrefix prefix = maybe "" (\a -> prefix ++ show a)

showMaybePostfix :: Show a => String -> Maybe a -> String
showMaybePostfix postfix = maybe "" (\a -> show a ++ postfix)

-------------------- Scheme --------------------

newtype Scheme = Scheme {_schemeContent :: T.Text} deriving (Eq)

schemeContent :: Lens' Scheme T.Text
schemeContent = lens _schemeContent (const mkScheme)

mkScheme :: T.Text -> Scheme
mkScheme s = Scheme (T.toLower s)

instance Show Scheme where
  show = T.unpack . _schemeContent

-------------------- UserInfo --------------------

newtype UserInfo = UserInfo {_userInfoText :: T.Text} deriving (Eq)

makeLenses ''UserInfo

instance Show UserInfo where
  --Don't show anything after the first colon because passwords are sometimes located there
  show (UserInfo str) = T.unpack $ T.takeWhile (/= ':') str

-------------------- Host --------------------

data Host
  = HostIPLiteral {_ipLiteralText :: T.Text}
  | HostIPv4 {_ipv4Text :: T.Text}
  | HostRegName {_hostName :: T.Text}
  deriving (Eq)

makeLenses ''Host

instance Show Host where
  show (HostIPLiteral str) = "[" ++ T.unpack str ++ "]"
  show (HostIPv4 str) = T.unpack str
  show (HostRegName str) = T.unpack str

-------------------- Port --------------------

newtype Port = Port {_portNum :: Int} deriving (Eq)

makeLenses ''Port

instance Show Port where
  show (Port i) = show i

-------------------- Authority --------------------

data Authority = Authority {_userInfo :: Maybe UserInfo, _host :: Host, _port :: Maybe Port} deriving (Eq)

makeLenses ''Authority

instance Show Authority where
  show (Authority userInfo' host' port') = concat [showMaybePostfix "@" userInfo', show host', showMaybePrefix ":" port']

-------------------- HierPart --------------------

data HierPart
  = HierAbEmpty {_hierAuthority :: Authority, _hierSegments :: [T.Text]}
  | HierAbsolute {_hierSegments :: [T.Text]}
  | HierRootless {_hierFirst :: T.Text, _hierSegments :: [T.Text]}
  | HierEmptyPath
  deriving (Eq)

makeLenses ''HierPart

instance Show HierPart where
  show (HierAbEmpty authority segments) = show authority ++ "/" ++ intercalate "/" (fmap T.unpack segments)
  show (HierAbsolute segments) = "/" ++ intercalate "/" (fmap T.unpack segments)
  show (HierRootless first segments) = T.unpack first ++ intercalate "/" (fmap T.unpack segments)
  show HierEmptyPath = ""

-------------------- RelativePart --------------------

data RelativePart
  = RelAbEmpty {_relAuthority :: Authority, _relSegments :: [T.Text]}
  | RelAbsolute {_relSegments :: [T.Text]}
  | RelNoScheme {_relFirst :: T.Text, _relSegments :: [T.Text]}
  | RelEmptyPath
  deriving (Eq)

makeLenses ''RelativePart

instance Show RelativePart where
  show (RelAbEmpty authority segments) = show authority ++ "/" ++ intercalate "/" (fmap T.unpack segments)
  show (RelAbsolute segments) = "/" ++ intercalate "/" (fmap T.unpack segments)
  show (RelNoScheme first segments) = T.unpack first ++ intercalate "/" (fmap T.unpack segments)
  show RelEmptyPath = ""

-------------------- Path --------------------

data Path
  = PathAbEmpty {_pathSegments :: [T.Text]}
  | PathAbsolute {_pathSegments :: [T.Text]}
  | PathNoScheme {_pathFirst :: T.Text, _pathSegments :: [T.Text]}
  | PathRootless {_pathFirst :: T.Text, _pathSegments :: [T.Text]}
  | PathEmpty
  deriving (Eq)

makeLenses ''Path

instance Show Path where
  show (PathAbEmpty segments) = "/" ++ intercalate "/" (fmap T.unpack segments)
  show (PathAbsolute segments) = "/" ++ intercalate "/" (fmap T.unpack segments)
  show (PathNoScheme first segments) = T.unpack first ++ intercalate "/" (fmap T.unpack segments)
  show (PathRootless first segments) = T.unpack first ++ intercalate "/" (fmap T.unpack segments)
  show PathEmpty = ""

pathOfHierPart :: HierPart -> Path
pathOfHierPart (HierAbEmpty _ segments) = PathAbEmpty segments
pathOfHierPart (HierAbsolute segments) = PathAbsolute segments
pathOfHierPart (HierRootless first segments) = PathRootless first segments
pathOfHierPart HierEmptyPath = PathEmpty

pathOfRelPart :: RelativePart -> Path
pathOfRelPart (RelAbEmpty _ segments) = PathAbEmpty segments
pathOfRelPart (RelAbsolute segments) = PathAbsolute segments
pathOfRelPart (RelNoScheme first segments) = PathNoScheme first segments
pathOfRelPart RelEmptyPath = PathEmpty

-------------------- Query --------------------

newtype Query = Query {_queryText :: T.Text} deriving (Eq)

makeLenses ''Query

instance Show Query where
  show (Query q) = T.unpack q

-------------------- Fragment --------------------

newtype Fragment = Fragment {_fragmentText :: T.Text} deriving (Eq)

makeLenses ''Fragment

instance Show Fragment where
  show (Fragment f) = T.unpack f

-------------------- Uri --------------------

--TODO: Normalize the URI with percent-encoding, case and syntax based encoding before comparing them
data Uri = Uri
  { _scheme :: Scheme,
    _hierPart :: HierPart,
    _query :: Maybe Query,
    _fragment :: Maybe Fragment
  }
  deriving (Eq)

makeLenses ''Uri

instance Show Uri where
  show (Uri scheme' hierPart' query' fragment') =
    concat [show scheme', ":", show hierPart', showMaybePrefix "?" query', showMaybePrefix "#" fragment']

-------------------- UriReference --------------------

data UriReference = UriRef Uri | RelativeRef RelativePart (Maybe Query) (Maybe Fragment)

instance Show UriReference where
  show (UriRef uri) = show uri
  show (RelativeRef relativePart query' fragment') =
    concat [show relativePart, showMaybePrefix "?" query', showMaybePrefix "#" fragment']
