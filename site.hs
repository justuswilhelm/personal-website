--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid (mappend)
import           Hakyll

import qualified Compilers   as C
import qualified Contexts    as Ctx

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
    match "mscgen-images/*.svg" $ do
      route idRoute
      compile copyFileCompiler
    match "graphviz-images/*.svg" $ do
      route idRoute
      compile copyFileCompiler
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
    match "posts/*" $ do
      version "pdf" $ do
        route $ setExtension "pdf"
        -- todo find a way we can pass the context here
        compile $ do
          body@(Item _ _) <- getResourceBody
          readPandocWith defaultHakyllReaderOptions body >>=
            C.relativizeUrlsWithCompiler "." >>=
            C.traverseRenderAll >>=
            C.writePandocLatexWith body
      version "full" $ do
        route $ setExtension "html"
        compile $
          C.customPostPandocCompiler >>=
          loadAndApplyTemplate "templates/post.html" Ctx.postCtx >>=
          loadAndApplyTemplate "templates/default.html" Ctx.postCtx >>=
          saveSnapshot "content"
      version "teaser" $ do
        route $ setExtension "toc-html"
        compile $ C.customTeaserPandocCompiler >>= saveSnapshot "content"
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <-
          recentFirst =<<
          loadAllSnapshots ("posts/*" .&&. hasVersion "teaser") "content"
        let indexCtx =
              listField "posts" Ctx.teaserCtx (return posts) `mappend`
              constField "title" "Home" `mappend`
              Ctx.pageDefaultContext
        getResourceBody >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/default.html" indexCtx >>=
          C.replaceTocExtension >>=
          relativizeUrls
    match "templates/*" $ compile templateBodyCompiler
    match "robots.txt" $ do
      route idRoute
      compile $ getResourceBody >>= relativizeUrls
    create ["sitemap.xml"] $ do
      route idRoute
      compile $ do
        posts <-
          recentFirst =<<
          loadAllSnapshots ("posts/*" .&&. hasVersion "full") "content"
        pages <- loadAll "pages/*"
        let allPosts = return (pages ++ posts)
        let sitemapCtx =
              mconcat
                [ listField "entries" Ctx.postCtx allPosts
                , Ctx.pageDefaultContext
                ]
        makeItem "" >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = Ctx.postCtx `mappend` bodyField "description"
                  -- constField "description" "This is the post description"
        posts <-
          fmap (take 10) . recentFirst =<<
          loadAllSnapshots ("posts/*" .&&. hasVersion "full") "content"
        renderAtom feedConfiguration feedCtx posts

--------------------------------------------------------------------------------
--- For RSS
feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = Ctx.pageTitle
    , feedDescription = "Articles about software and life"
    , feedAuthorName = Ctx.authorName
    , feedAuthorEmail = "hello@justus.pw"
    , feedRoot = Ctx.baseUrl
    }
