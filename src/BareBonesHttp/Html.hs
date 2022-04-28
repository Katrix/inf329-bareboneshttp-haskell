{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module BareBonesHttp.Html where

import BareBonesHttp.Http.AsResponse
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

data TagBuilder = T T.Text [TagBuilder] | A T.Text T.Text | S T.Text

type Html = [TagBuilder]

instance AsResponse [TagBuilder] where
  contentType _ = "text/html; charset=UTF-8"

  encodeBody a = TE.encodeUtf8 $ tagBuilderToText (T "html" a)

tagBuilderToText :: TagBuilder -> T.Text
tagBuilderToText (T tag content) =
  ("<" <> escapedTag <> ">")
    <> foldMap tagBuilderToText content
    <> ("</" <> escapedTag <> ">")
  where
    escapedTag = escapeHtml tag
tagBuilderToText (A attrib value) = escapeHtml attrib <> "=" <> escapeHtml value
tagBuilderToText (S content) = escapeHtml content

escapeHtml :: T.Text -> T.Text
escapeHtml =
  T.replace "&" "&amp;"
    . T.replace "<" "&lt;"
    . T.replace ">" "&gt;"
    . T.replace "\"" "&quot;"
    . T.replace "'" "&#39;"

div :: [TagBuilder] -> TagBuilder
div = T "div"

p :: [TagBuilder] -> TagBuilder
p = T "p"

cls :: T.Text -> TagBuilder
cls = A "class"

--TODO
