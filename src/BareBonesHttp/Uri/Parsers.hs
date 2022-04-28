{-# LANGUAGE OverloadedStrings #-}

module BareBonesHttp.Uri.Parsers
  (
    Parser,
    uri,
    hierPart,
    uriReference,
    relativeRef,
    relativePart,
    scheme,
    authority,
    userInfo,
    host,
    port,
    ipLiteral,
    ipvFuture,
    ipv6Address,
    ipv4Address,
    ipv4AddressRaw,
    regName,
    pathAbEmpty,
    pathAbsolute,
    pathNoScheme,
    pathRootless,
    segment,
    segmentNz,
    segmentNzNc,
    pchar,
    query,
    fragment,
    pctEncoded,
    unreserved,
    reserved,
    genDelims,
    subDelims,
    hexDig,
    alpha,
    digit
  )
where

import BareBonesHttp.Uri.Definitions hiding (scheme, hierPart, query, fragment, userInfo, host, port)
import Control.Applicative hiding (many, some)
import Data.Char (ord)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void T.Text

liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = liftA2 f a b <*> c <*> d

charBetween :: Int -> Int -> Parser Char
charBetween fromIdx toIdx = satisfy (\c -> ord c >= fromIdx && ord c <= toIdx)

uri :: Parser Uri
uri = liftA4 Uri (try (scheme <* char ':')) hierPart (optional (char '?' *> query)) (optional (char '#' *> fragment))

hierPart :: Parser HierPart
hierPart =
  choice
    [ liftA2 HierAbEmpty (string "//" *> authority) pathAbEmpty,
      fmap HierAbsolute pathAbsolute,
      fmap (uncurry HierRootless) pathRootless,
      pure HierEmptyPath
    ]

uriReference :: Parser UriReference
uriReference = UriRef <$> uri <|> relativeRef

relativeRef :: Parser UriReference
relativeRef = liftA3 RelativeRef relativePart (optional (char '?' *> query)) (optional (char '#' *> fragment))

relativePart :: Parser RelativePart
relativePart =
  choice
    [ liftA2 RelAbEmpty (string "//" *> authority) pathAbEmpty,
      fmap RelAbsolute pathAbsolute,
      fmap (uncurry RelNoScheme) pathNoScheme,
      pure RelEmptyPath
    ]

scheme :: Parser Scheme
scheme = liftA2 (\h t -> mkScheme (T.pack (h : t))) alpha (some (choice [alpha, digit, char '+', char '-', char '.']))

authority :: Parser Authority
authority = liftA3 Authority (optional (try (userInfo <* string "@"))) host (optional (string ":" *> port))

userInfo :: Parser UserInfo
userInfo = UserInfo . T.concat <$> many (choice [unreserved, pctEncoded, subDelims, string ":"])

host :: Parser Host
host = choice [ipLiteral, try ipv4Address, regName]

port :: Parser Port
port = Port . read <$> many digit

ipLiteral :: Parser Host
ipLiteral = HostIPLiteral <$> (char '[' *> (ipv6Address <|> ipvFuture) <* char ']')

concatSeq :: [Parser T.Text] -> Parser T.Text
concatSeq ps = T.concat <$> sequence ps

concatCount :: Int -> Parser T.Text -> Parser T.Text
concatCount n p = T.concat <$> count n p

concatCount' :: Int -> Int -> Parser T.Text -> Parser T.Text
concatCount' fromNum toNum p = T.concat <$> count' fromNum toNum p

ipvFuture :: Parser T.Text
ipvFuture =
  concatSeq
    [ string "v",
      T.pack <$> some hexDig,
      string "*",
      T.concat <$> some (unreserved <|> subDelims <|> string ":")
    ]

-- Deliberately verbose so it's easier to confirm that it encodes the
-- BNF for an IPV6 address found in RFC3986
ipv6Address :: Parser T.Text
ipv6Address =
  choice
    [ try $ cs [{-                                                      -} cc 6 (cs [h16, string ":"]), ls32],
      try $ cs [{-                                         -} string "::", cc 5 (cs [h16, string ":"]), ls32],
      try $ cs [opt {-                               -} h16, string "::", cc 4 (cs [h16, string ":"]), ls32],
      try $ cs [opt (cs [cco 1 (cs [h16, string ":"]), h16]), string "::", cc 3 (cs [h16, string ":"]), ls32],
      try $ cs [opt (cs [cco 2 (cs [h16, string ":"]), h16]), string "::", cc 2 (cs [h16, string ":"]), ls32],
      try $ cs [opt (cs [cco 3 (cs [h16, string ":"]), h16]), string "::" {- -}, cs [h16, string ":"], ls32],
      try $ cs [opt (cs [cco 4 (cs [h16, string ":"]), h16]), string "::" {-                        -}, ls32],
      try $ cs [opt (cs [cco 5 (cs [h16, string ":"]), h16]), string "::" {-                        -}, h16],
      {-  -} cs [opt (cs [cco 6 (cs [h16, string ":"]), h16]), string "::"]
    ]
  where
    h16 :: Parser T.Text
    h16 = T.pack <$> count' 1 4 hexDig
    ls32 :: Parser T.Text
    ls32 = concatSeq [h16, string ":", h16] <|> ipv4AddressRaw
    opt p = option "" p
    cs = concatSeq
    cc = concatCount
    cco maxC p = concatCount' 0 maxC p

ipv4Address :: Parser Host
ipv4Address = HostIPv4 <$> ipv4AddressRaw

ipv4AddressRaw :: Parser T.Text
ipv4AddressRaw =
  concatSeq [pDecOctet, string ".", pDecOctet, string ".", pDecOctet, string ".", pDecOctet]
  where
    pDecOctet =
      choice
        [ T.singleton <$> digit,
          T.pack <$> sequence [choice $ fmap char ['1', '2', '3', '4', '5', '6', '7', '8', '9'], digit],
          T.pack <$> sequence [char '1', digit, digit],
          T.pack <$> sequence [char '2', choice $ fmap char ['0', '1', '2', '3', '4'], digit],
          T.pack <$> sequence [char '2', char '5', choice $ fmap char ['0', '1', '2', '3', '4', '5']]
        ]

regName :: Parser Host
regName = HostRegName . T.concat <$> many (unreserved <|> pctEncoded <|> subDelims)

pathAbEmpty :: Parser [T.Text]
pathAbEmpty = many (char '/' *> segment)

pathAbsolute :: Parser [T.Text]
pathAbsolute = char '/' *> option [] (liftA2 (:) segmentNz (many (char '/' *> segment)))

pathNoScheme :: Parser (T.Text, [T.Text])
pathNoScheme = liftA2 (,) segmentNzNc (many (char '/' *> segment))

pathRootless :: Parser (T.Text, [T.Text])
pathRootless = liftA2 (,) segmentNz (many (char '/' *> segment))

segment :: Parser T.Text
segment = T.concat <$> many pchar

segmentNz :: Parser T.Text
segmentNz = T.concat <$> some pchar

segmentNzNc :: Parser T.Text
segmentNzNc = T.concat <$> some (unreserved <|> pctEncoded <|> subDelims <|> string "@")

pchar :: Parser T.Text
pchar = unreserved <|> pctEncoded <|> subDelims <|> string ":" <|> string "@"

query :: Parser Query
query = Query . T.concat <$> many (pchar <|> string "/" <|> string "?")

fragment :: Parser Fragment
fragment = Fragment . T.concat <$> many (pchar <|> string "/" <|> string "?")

pctEncoded :: Parser T.Text
pctEncoded = liftA3 (\a b c -> T.pack [a, b, c]) (char '%') hexDig hexDig

unreserved :: Parser T.Text
unreserved = T.singleton <$> choice [alpha, digit, char '-', char '.', char '_', char '~']

reserved :: Parser T.Text
reserved = genDelims <|> subDelims

genDelims :: Parser T.Text
genDelims = fmap T.singleton (choice $ fmap char [':', '/', '?', '#', '[', ']', '@'])

subDelims :: Parser T.Text
subDelims = fmap T.singleton (choice $ fmap char ['!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '='])

hexDig :: Parser Char
hexDig = digit <|> choice (fmap char ['A', 'B', 'C', 'D', 'E', 'F'])

alpha :: Parser Char
alpha = charBetween 0x41 0x5A <|> charBetween 0x61 0x7A

digit :: Parser Char
digit = charBetween 0x30 0x39
