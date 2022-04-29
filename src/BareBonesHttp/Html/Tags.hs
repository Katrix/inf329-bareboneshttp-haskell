{-# LANGUAGE OverloadedStrings #-}

module BareBonesHttp.Html.Tags where

import BareBonesHttp.Html

head :: Html -> TagBuilder
head = tag "head"

meta :: Html -> TagBuilder
meta = metaTag "meta"

title :: Html -> TagBuilder
title = tag "title"

link :: Html -> TagBuilder
link = tag "link"

script :: Html -> TagBuilder
script = tag "script"

body :: Html -> TagBuilder
body = tag "body"

div :: Html -> TagBuilder
div = tag "div"

p :: Html -> TagBuilder
p = tag "p"

a :: Html -> TagBuilder
a = tag "a"

span :: Html -> TagBuilder
span = tag "span"

h1 :: Html -> TagBuilder
h1 = tag "h1"

h2 :: Html -> TagBuilder
h2 = tag "h2"

h3 :: Html -> TagBuilder
h3 = tag "h3"

h4 :: Html -> TagBuilder
h4 = tag "h4"

h5 :: Html -> TagBuilder
h5 = tag "h5"

h6 :: Html -> TagBuilder
h6 = tag "h6"

ul :: Html -> TagBuilder
ul = tag "ul"

ulH :: Html -> [Html] -> TagBuilder
ulH attribs elems = ul (attribs ++ fmap li elems)

li :: Html -> TagBuilder
li = tag "li"

nav :: Html -> TagBuilder
nav = tag "nav"

form :: Html -> TagBuilder
form = tag "form"

button :: Html -> TagBuilder
button = tag "button"

footer :: Html -> TagBuilder
footer = tag "footer"
