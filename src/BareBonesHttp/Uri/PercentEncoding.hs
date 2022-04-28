{-# LANGUAGE OverloadedStrings #-}

module BareBonesHttp.Uri.PercentEncoding where

import BareBonesHttp.Uri.Definitions
import Control.Applicative
import Control.Lens
import Data.Char (isAlphaNum)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as B
import Numeric

--TODO: Use better hex and unhex functions

percentEncode :: T.Text -> T.Text
percentEncode text = TE.decodeUtf8 (B.concatMap encodeCharIfNeeded (TE.encodeUtf8 text))
  where
    encodeCharIfNeeded c = if charNeedsEncoding c then encodeChar c else B.singleton c
    charNeedsEncoding :: Char -> Bool
    charNeedsEncoding c = not (isAlphaNum c) && c `notElem` ("-._~" :: String)
    encodeChar c = B.singleton '%' <> padStart 2 '0' (hexOctet c)
    hexOctet c = TE.encodeUtf8 (T.pack $ showHex (fromEnum c) "")
    padStart minLength a l = B.replicate (minLength - B.length l) a <> l

percentDecode :: T.Text -> T.Text
percentDecode text = case B.split '%' (TE.encodeUtf8 text) of
  [] -> ""
  [_] -> text
  h : t -> TE.decodeUtf8 (B.concat $ h : fmap decodePercentEncoding t)
  where
    decodePercentEncoding segment =
      if B.length segment >= 2
        then
          let (hex, rest) = B.splitAt 2 segment
           in maybe segment (\c -> B.singleton c <> rest) (hexToChar hex)
        else segment
    hexToChar hex = case readHex (T.unpack $ TE.decodeUtf8 hex) of
      [(n, "")] -> Just $ toEnum n
      _ -> Nothing

hostPercentEncoded :: Traversal' Host T.Text
hostPercentEncoded = hostName

authorityPercentEncoded :: Traversal' Authority T.Text
authorityPercentEncoded focus (Authority u h p) = liftA3 Authority ((_Just . userInfoText) focus u) (hostPercentEncoded focus h) (pure p)

hierPartPercentEncoded :: Traversal' HierPart T.Text
hierPartPercentEncoded focus (HierAbEmpty a s) = liftA2 HierAbEmpty (authorityPercentEncoded focus a) (traverse focus s)
hierPartPercentEncoded focus (HierAbsolute s) = fmap HierAbsolute (traverse focus s)
hierPartPercentEncoded focus (HierRootless f s) = liftA2 HierRootless (focus f) (traverse focus s)
hierPartPercentEncoded _ HierEmptyPath = pure HierEmptyPath

relativePartPercentEncoded :: Traversal' RelativePart T.Text
relativePartPercentEncoded focus (RelAbEmpty a s) = liftA2 RelAbEmpty (authorityPercentEncoded focus a) (traverse focus s)
relativePartPercentEncoded focus (RelAbsolute s) = fmap RelAbsolute (traverse focus s)
relativePartPercentEncoded focus (RelNoScheme f s) = liftA2 RelNoScheme (focus f) (traverse focus s)
relativePartPercentEncoded _ RelEmptyPath = pure RelEmptyPath

pathPercentEncoded :: Traversal' Path T.Text
pathPercentEncoded focus (PathAbEmpty s) = fmap PathAbEmpty (traverse focus s)
pathPercentEncoded focus (PathAbsolute s) = fmap PathAbsolute (traverse focus s)
pathPercentEncoded focus (PathRootless f s) = liftA2 PathRootless (focus f) (traverse focus s)
pathPercentEncoded focus (PathNoScheme f s) = liftA2 PathNoScheme (focus f) (traverse focus s)
pathPercentEncoded _ PathEmpty = pure PathEmpty

uriPercentEncoded :: Traversal' Uri T.Text
uriPercentEncoded focus (Uri s hier q f) =
  liftA4 Uri (pure s) (hierPartPercentEncoded focus hier) ((_Just . queryText) focus q) ((_Just . fragmentText) focus f)
  where
    liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
    liftA4 f' a b c d = liftA2 f' a b <*> c <*> d

percentEncodeUri :: Uri -> Uri
percentEncodeUri = over uriPercentEncoded percentEncode

percentDecodeUri :: Uri -> Uri
percentDecodeUri = over uriPercentEncoded percentDecode

uriReferencePercentEncoded :: Traversal' UriReference T.Text
uriReferencePercentEncoded focus (UriRef ref) = fmap UriRef (uriPercentEncoded focus ref)
uriReferencePercentEncoded focus (RelativeRef p q f) =
  liftA3 RelativeRef (relativePartPercentEncoded focus p) ((_Just . queryText) focus q) ((_Just . fragmentText) focus f)
