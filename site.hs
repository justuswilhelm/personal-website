--------------------------------------------------------------------------------
import Hakyll

import qualified Compilers as C
import qualified Contexts as Ctx

--------------------------------------------------------------------------------
main :: IO ()
main =
  hakyll $ do
    match (fromGlob "static/**") $ do
      route idRoute
      compile copyFileCompiler
    match (fromGlob "css/*") $ do
      route idRoute
      compile compressCssCompiler
    match (fromGlob "posts/*") $
      version "html" $ do
        route $ setExtension "html"
        compile $
          C.customPostPandocCompiler >>=
          loadAndApplyTemplate (fromFilePath "templates/post.html") Ctx.postCtx >>=
          saveSnapshot "atom" >>=
          loadAndApplyTemplate
            (fromFilePath "templates/default.html")
            Ctx.postCtx >>=
          saveSnapshot "content"
    match (fromGlob "posts/*") $
      version "pdf" $ do
        route $ setExtension "pdf"
      -- todo find a way we can pass the context here
        compile $ do
          body <- getResourceBody
          readPandocWith defaultHakyllReaderOptions body >>=
            C.relativizeUrlsWithCompiler "." >>=
            C.traverseRenderAll >>=
            C.writePandocLatexWith body
    match (fromGlob "posts/*") $
      version "teaser" $
      -- A little bit hacky, it generates a toc-html file which we don't need
      -- The only reason we create it is that we want to have some
      -- post link the we can direct to when showing this in the index
       do
        route $ setExtension "toc-html"
        compile $ C.customTeaserPandocCompiler >>= saveSnapshot "content"
    match (fromGlob "index.html") $ do
      route idRoute
      compile $ do
        posts <-
          recentFirst =<<
          loadAllSnapshots
            (fromGlob "posts/*" .&&. hasVersion "teaser")
            "content"
        let indexCtx =
              listField "posts" Ctx.teaserCtx (return posts) `mappend`
              constField "title" "Home" `mappend`
              Ctx.pageDefaultContext
        getResourceBody >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate (fromFilePath "templates/default.html") indexCtx >>=
          C.replaceTocExtension >>=
          relativizeUrls
    match (fromGlob "templates/*") $ compile templateBodyCompiler
    match (fromGlob "robots.txt") $ do
      route idRoute
      compile $ getResourceBody >>= relativizeUrls
    create [fromFilePath "sitemap.xml"] $ do
      route idRoute
      compile $ do
        posts <-
          recentFirst =<<
          loadAllSnapshots (fromGlob "posts/*" .&&. hasVersion "html") "content"
        -- TODO add pdf to sitempa
        -- postsPdf <-
        --   recentFirst =<<
        --   loadAll ("posts/*" .&&. hasVersion "pdf")
        -- we get this error right now
        -- [ERROR] Hakyll.Core.Compiler.Require.load:
        -- posts/2015-09-10-post.md (pdf) (snapshot _final) was found in the
        -- cache, but does not have the right type: expected [Char] but got
        -- ByteString
        pages <- loadAll $ fromGlob "pages/*"
        let allPosts = return (pages ++ posts)
        let sitemapCtx =
              mconcat
                [ listField "entries" Ctx.postCtx allPosts
                , Ctx.pageDefaultContext
                ]
        makeItem "" >>=
          loadAndApplyTemplate (fromFilePath "templates/sitemap.xml") sitemapCtx
    create [fromFilePath "atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = Ctx.postCtx `mappend` bodyField "description"
        posts <-
          recentFirst =<<
          loadAllSnapshots (fromGlob "posts/*" .&&. hasVersion "html") "atom"
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
