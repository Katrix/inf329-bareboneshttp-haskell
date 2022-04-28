module BareBonesHttp.Bidi where

import Control.Arrow

data Bidi a ti to bi bo = Bidi
  { bidiTop :: a ti to,
    bidiBottom :: a bi bo
  }

type SimpleBidi a i o = Bidi a i o i o

bidi :: a b c -> SimpleBidi a b c
bidi f = Bidi f f

idBidi :: Arrow a => SimpleBidi a b b
idBidi = bidi (arr id)

bidiFlip :: Bidi a ti to bi bo -> Bidi a bi bo ti to
bidiFlip (Bidi top bottom) = Bidi bottom top

(.|) :: Arrow a => Bidi a ti tm bm bo -> Bidi a tm to bi bm -> Bidi a ti to bi bo
(Bidi top1 bottom1) .| (Bidi top2 bottom2) =
  Bidi
    (top1 >>> top2)
    (bottom1 <<< bottom2)

(.||) :: Arrow a => Bidi a ti tm bm bo -> a tm bm -> a ti bo
(Bidi top bottom) .|| finalStep = top >>> finalStep >>> bottom

onTop :: (a ti to -> a fti fto) -> Bidi a ti to bi bo -> Bidi a fti fto bi bo
onTop f (Bidi top bottom) = Bidi (f top) bottom

onBottom :: (a bi bo -> a fbi fbo) -> Bidi a ti to bi bo -> Bidi a ti to fbi fbo
onBottom f (Bidi top bottom) = Bidi top (f bottom)

onTopLeft :: Arrow a => a ti tm -> Bidi a tm to bi bo -> Bidi a ti to bi bo
onTopLeft a = onTop (a >>>)

onTopRight :: Arrow a => Bidi a ti tm bi bo -> a tm to -> Bidi a ti to bi bo
onTopRight b a = onTop (>>> a) b

onBottomLeft :: Arrow a => a bi bm -> Bidi a ti to bm bo -> Bidi a ti to bi bo
onBottomLeft a = onBottom (a >>>)

onBottomRight :: Arrow a => Bidi a ti to bi bm -> a bm bo -> Bidi a ti to bi bo
onBottomRight b a = onBottom (>>> a) b
