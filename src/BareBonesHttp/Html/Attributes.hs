{-# LANGUAGE OverloadedStrings #-}

module BareBonesHttp.Html.Attributes where

import BareBonesHttp.Html
import qualified Data.Text as T

cls :: T.Text -> TagBuilder
cls = A "class"

style :: T.Text -> TagBuilder
style = A "style"

charset :: T.Text -> TagBuilder
charset = A "charset"

viewport :: T.Text -> TagBuilder
viewport = A "viewport"

href :: T.Text -> TagBuilder
href = A "href"

rel :: T.Text -> TagBuilder
rel = A "rel"

integrity :: T.Text -> TagBuilder
integrity = A "integrity"

crossorigin :: T.Text -> TagBuilder
crossorigin = A "crossorigin"

src :: T.Text -> TagBuilder
src = A "src"

id :: T.Text -> TagBuilder
id = A "id"

tpe :: T.Text -> TagBuilder
tpe = A "tpe"

dataA :: T.Text -> T.Text -> TagBuilder
dataA d = A ("data-" <> d)

action :: T.Text -> TagBuilder
action = A "action"

method :: T.Text -> TagBuilder
method = A "method"
