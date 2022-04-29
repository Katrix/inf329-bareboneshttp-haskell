{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BareBonesHttp.Html where

import BareBonesHttp.Http.AsResponse
import Data.List (partition)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

data TagBuilder = T T.Text Bool [TagBuilder] | A T.Text T.Text | S T.Text

type Html = [TagBuilder]

tag :: T.Text -> Html -> TagBuilder
tag t = T t True

metaTag :: T.Text -> Html -> TagBuilder
metaTag t = T t False

instance AsResponse [TagBuilder] where
  contentType _ = "text/html; charset=UTF-8"

  encodeBody a = TE.encodeUtf8 ("<!doctype html>\n" <> tagBuilderToText (tag "html" a))

tagBuilderToText :: TagBuilder -> T.Text
tagBuilderToText (T tag addEndTag content) =
  if addEndTag
    then
      tagWithAttribs
        <> foldMap tagBuilderToText elements
        <> ("</" <> escapedTag <> ">")
    else tagWithAttribs
  where
    tagWithAttribs = "<" <> escapedTag <> attribTexts <> ">"
    escapedTag = escapeHtml tag
    (attribs, elements) =
      partition
        ( \case
            A _ _ -> True
            _ -> False
        )
        content
    attribTexts = foldMap (\t -> " " <> tagBuilderToText t) attribs
tagBuilderToText (A attrib value) = escapeHtml attrib <> "=\"" <> escapeHtml value <> "\""
tagBuilderToText (S content) = escapeHtml content

escapeHtml :: T.Text -> T.Text
escapeHtml =
  T.replace "&" "&amp;"
    . T.replace "<" "&lt;"
    . T.replace ">" "&gt;"
    . T.replace "\"" "&quot;"
    . T.replace "'" "&#39;"
