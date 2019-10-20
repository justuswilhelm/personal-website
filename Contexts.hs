module Contexts
  ( baseUrl
  , pageTitle
  , authorName
  , pageDefaultContext
  , postCtx
  , teaserCtx
  ) where

import           Hakyll

baseUrl :: String
baseUrl = "https://www.justus.pw"

pageTitle :: String
pageTitle = "Justus Perlwitz"

authorName :: String
authorName = "Justus Perlwitz"

pageDefaultContext :: Context String
pageDefaultContext =
  constField "baseUrl" baseUrl `mappend` constField "pageTitle" pageTitle `mappend`
  constField "authorName" authorName `mappend`
  defaultContext

postCtx :: Context String
postCtx =
  mapContext replacePdfWithHtml $
  urlField "pdfUrl" `mappend` dateField "lastmod" "%Y-%m-%d" `mappend`
  dateField "date" "%B %e, %Y" `mappend`
  pageDefaultContext
  where
    replacePdfWithHtml = replaceAll "\\.html" (const ".pdf")

teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" `mappend` postCtx
