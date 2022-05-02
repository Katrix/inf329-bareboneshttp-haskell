{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import BareBonesHttp.Bidi
import BareBonesHttp.Html
import qualified BareBonesHttp.Html.Attributes as HA
import qualified BareBonesHttp.Html.Tags as HT
import BareBonesHttp.Http
import BareBonesHttp.Http.LogTimeSpent
import BareBonesHttp.Http.SecurityHeaders
import Control.Concurrent.STM
import Control.Monad.Error.Class
import Control.Monad.State.Class
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Text as T

main :: IO ()
main = do
  counter <- newTVarIO (0 :: Int)
  runStdoutLoggingT $
    filterLogger (\_ level -> level >= LevelInfo) $
      runServerRoutes
        Nothing
        "9000"
        (HttpAuthority (HostRegName "localhost") (Just (Port (Just 9000))))
        (addSecurityHeaders .| logTimeSpent)
        (applicationRoutes counter)

applicationRoutes :: (MonadIO m, MonadLogger m, MonadError (Response ()) m, HasCap StartedProcessing c) => TVar Int -> RouteHandler m c
applicationRoutes counter =
  routes
    simpleNotFound
    [ handle
        (match [Get] noSegmentsRemaining)
        (\req _ -> ok req . renderHomePage <$> liftIO (readTVarIO counter)),
      handle
        (match [Get] (exact "other-page"))
        (\req _ -> pure $ ok req renderOtherPage),
      handle
        (match [Get] (exact "assets" *> remaining1))
        (\req path -> serveFiles "resources/" req (uncurry (:) path)),
      handle
        (match [Post] (exact "counter" *> exact "increment" *> matchInt))
        ( \req num -> do
            counterState <- liftIO (atomically (stateTVar counter (dup . (num +))))
            pure $ ok req (renderHomePage counterState)
        ),
      handle
        (match [Post] (exact "counter" *> exact "decrement" *> matchInt))
        ( \req num -> do
            counterState <- liftIO (atomically (stateTVar counter (dup . subtract num)))
            pure $ ok req (renderHomePage counterState)
        )
    ]
  where
    dup a = (a, a)

headerLayout :: Html
headerLayout =
  [ HT.nav
      [ HA.cls "navbar navbar-expand-lg navbar-light bg-light",
        HT.div
          [ HA.cls "container",
            HT.a [HA.cls "navbar-brand", HA.href "/", S "SomeSite"],
            HT.button
              [ HA.cls "navbar-toggler",
                HA.tpe "button",
                HA.dataA "bs-toggle" "collapse",
                HA.dataA "bs-target" "#navbarSupportedContent",
                HT.span [HA.cls "navbar-toggler-icon"]
              ],
            HT.div
              [ HA.cls "collapse navbar-collapse",
                HA.id "navbarSupportedContent",
                HT.ulH
                  [HA.cls "navbar-nav me-auto mb-2 mb-lg-0"]
                  [ [HA.cls "nav-item", HT.a [HA.cls "nav-link", HA.href "/", S "Home"]],
                    [HA.cls "nav-item", HT.a [HA.cls "nav-link", HA.href "/other-page", S "Other page"]]
                  ]
              ]
          ]
      ]
  ]

footerLayout :: Html
footerLayout =
  [ HT.footer
      [ HA.cls "site-footer",
        HT.p [S "Some content here"],
        HT.p [S "© Someone"]
      ]
  ]

data LayoutProps = LayoutProps
  { title :: T.Text,
    moreMeta :: Html,
    moreStyling :: Html,
    content :: Html,
    extraScripts :: Html
  }

simpleLayoutProps :: T.Text -> Html -> LayoutProps
simpleLayoutProps title' content' = LayoutProps title' [] [] content' []

baseLayout :: LayoutProps -> Html
baseLayout (LayoutProps title' moreMeta' moreStyling' content' extraScripts') =
  [ A "lang" "en",
    HT.head $
      [ HT.meta [HA.charset "utf-8"],
        HT.title [S title'],
        HT.meta [HA.viewport "width=device-width, initial-scale=1"]
      ]
        ++ moreMeta'
        ++ [ HT.link
               [ HA.href "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css",
                 HA.rel "stylesheet",
                 HA.integrity
                   "sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3",
                 HA.crossorigin "anonymous"
               ],
             HT.link [HA.href "/assets/app.css", HA.rel "stylesheet"]
           ]
        ++ moreStyling',
    HT.body $
      [ HT.div $
          [HA.cls "site"]
            ++ headerLayout
            ++ [ HT.div
                   [ HA.cls "site-content",
                     HT.div $ HA.cls "container container-pad" : content'
                   ]
               ]
            ++ footerLayout
      ]
        ++ [ HT.script
               [ HA.src "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js",
                 HA.integrity "sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p",
                 HA.crossorigin "anonymous"
               ]
           ]
        ++ extraScripts'
  ]

renderHomePage :: Int -> Html
renderHomePage counter =
  baseLayout $
    simpleLayoutProps
      "Home"
      [ HT.h1 [S "Home"],
        HT.div
          [ HA.style "display: flex",
            alterButtonStateButton "10" True,
            alterButtonStateButton "5" True,
            alterButtonStateButton "1" True,
            HT.p [HA.cls "h2", S ("Counter: " <> T.pack (show counter))],
            alterButtonStateButton "1" False,
            alterButtonStateButton "5" False,
            alterButtonStateButton "10" False
          ]
      ]
  where
    alterButtonStateButton n increment =
      HT.form
        [ HA.action ("/counter/" <> (if increment then "increment" else "decrement") <> "/" <> n),
          HA.method "POST",
          HT.button
            [ HA.tpe "submit",
              HA.cls "btn btn-primary",
              S (if increment then "+" <> n else "-" <> n)
            ]
        ]

renderOtherPage :: Html
renderOtherPage =
  baseLayout $
    simpleLayoutProps
      "Other page"
      [ HT.h1 [S "Other page"],
        HT.p [S "This is another page to show page navigation. Not much here sadly"]
      ]
