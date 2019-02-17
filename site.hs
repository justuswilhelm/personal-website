--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid          (mappend)
import           Hakyll

import           PandocFilterGraphviz (graphviz, msc)
import           Text.Pandoc          (Format (..), Pandoc, WriterOptions (..))
import           Text.Pandoc.Walk     (walkM)

import           Hakyll.Web.Html      (withUrls)

--------------------------------------------------------------------------------
main :: IO ()
main =
  hakyll $ do
    match "static/*" $ do
      route idRoute
      compile copyFileCompiler
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler
    match "mscgen-images/*.png" $ do
      route idRoute
      compile copyFileCompiler
    match "graphviz-images/*.svg" $ do
      route idRoute
      compile copyFileCompiler
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
    match "posts/*" $
      version "noToc" $ do
        route $ setExtension "toc-html"
        compile $ pandocCompiler >>= saveSnapshot "content"
            -- >>= relativizeUrls
    match "posts/*" $ do
      route $ setExtension "html"
      compile $
        customPandocCompiler >>=
        loadAndApplyTemplate "templates/post.html" postCtx >>=
        loadAndApplyTemplate "templates/default.html" postCtx
            -- >>= relativizeUrls
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <-
          recentFirst =<<
          loadAllSnapshots ("posts/*" .&&. hasVersion "noToc") "content"
        let indexCtx =
              listField "posts" teaserCtx (return posts) `mappend`
              constField "title" "Home" `mappend`
              pageDefaultContext
        getResourceBody >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/default.html" indexCtx >>=
          replaceTocExtension >>=
          relativizeUrls
    match "templates/*" $ compile templateBodyCompiler
    match "robots.txt" $ do
      route idRoute
      compile $ getResourceBody >>= relativizeUrls
    create ["sitemap.xml"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        pages <- loadAll "pages/*"
        let allPosts = return (pages ++ posts)
        let sitemapCtx =
              mconcat [listField "entries" postCtx allPosts, defaultContext]
        makeItem "" >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

--------------------------------------------------------------------------------
pageDefaultContext :: Context String
pageDefaultContext =
  constField "baseUrl" "https://www.justus.pw" `mappend`
  constField "pageTitle" "Justus Perlwitz" `mappend`
  defaultContext

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` pageDefaultContext

teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" `mappend` postCtx

---
woptions :: WriterOptions
woptions =
  defaultHakyllWriterOptions
    { writerSectionDivs = True
    , writerTableOfContents = True
    , writerColumns = 120
    , writerTemplate =
        Just
          "<div id=\"TOC\">$toc$</div>\n<div id=\"markdownBody\">$body$</div>"
    , writerTOCDepth = 4
    , writerHtmlQTags = True
    }

customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    woptions
    (unsafeCompiler . (walkM graphviz >> walkM msc))

replaceTocExtension :: Item String -> Compiler (Item String)
replaceTocExtension = return . fmap (withUrls replacer)
  where
    replacer = replaceAll "\\.toc-html" (const ".html")
