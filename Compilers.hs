-- Contain compilers used in this site
module Compilers
  ( customPostPandocCompiler
  , customTeaserPandocCompiler
  , replaceTocExtension
  , writePandocLatexWith
  , relativizeUrlsWithCompiler
  , traverseRenderAll
  ) where

import           Data.ByteString.Lazy (ByteString)

import           Hakyll               (Compiler, ContextField (..), Item (..),
                                       defaultHakyllReaderOptions,
                                       defaultHakyllWriterOptions, makeItem,
                                       pandocCompilerWithTransform,
                                       pandocCompilerWithTransformM, replaceAll,
                                       unContext, unsafeCompiler,
                                       unsafeCompiler, withUrls, withUrls)
import           Text.Pandoc          (Pandoc, WriterOptions (..), runIO,
                                       writeLaTeX)
import           Text.Pandoc.PDF      (makePDF)
import           Text.Pandoc.Walk     (walk, walkM)

import           PandocFilterGraphviz (RenderAllOptions (..), RenderFormat (..),
                                       relativizePandocUrls, renderAll,
                                       stripHeading)

import           Contexts             (postCtx)

postHakyllWriterOptions :: WriterOptions
postHakyllWriterOptions =
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

customPostPandocCompiler :: Compiler (Item String)
customPostPandocCompiler =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    postHakyllWriterOptions
    (unsafeCompiler . walkM (renderAll fmt))
  where
    fmt = RenderAllOptions {urlPrefix = Nothing, renderFormat = SVG}

customTeaserPandocCompiler :: Compiler (Item String)
customTeaserPandocCompiler =
  pandocCompilerWithTransform
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    (walk stripHeading)

replaceTocExtension :: Item String -> Compiler (Item String)
replaceTocExtension = return . fmap (withUrls replacer)
  where
    replacer = replaceAll "\\.toc-html" (const ".html")

-- Some code copy for pdf creation, taken from Hakyll.Web.Pandoc
writePandocLatexWith :: Item String -> Item Pandoc -> Compiler (Item ByteString)
writePandocLatexWith item (Item _ doc) = do
  author <- getStringFromContext "authorName"
  title <- getStringFromContext "title"
  date <- getStringFromContext "lastmod"
  let variables = [("author", author), ("title", title), ("date", date)]
  pdfString <-
    unsafeCompiler $ do
      template <- readFile "templates/post.tex"
      let options = pdfHakyllWriterOptions variables template
      pdfOrError <- runIO $ makePDF "xelatex" [] writeLaTeX options doc
      -- Homework: Find out why this is a nested Either
      case pdfOrError of
        Left err -> error $ "Main.writePandocLatexWith:" ++ show err
        Right success ->
          case success of
            Left err  -> error $ "Main.writePandocLatexWith:" ++ show err
            Right pdf -> return pdf
  makeItem pdfString
  where
    getString (StringField s) = return s
    getString (ListField _ _) = return ""
    getStringFromContext s = unContext postCtx s [] item >>= getString

relativizeUrlsWithCompiler :: String -> Item Pandoc -> Compiler (Item Pandoc)
relativizeUrlsWithCompiler _ item =
  return $ fmap (walk $ relativizePandocUrls ".") item

pdfHakyllWriterOptions :: [(String, String)] -> String -> WriterOptions
pdfHakyllWriterOptions options template =
  defaultHakyllWriterOptions
    { writerSectionDivs = True
    , writerTableOfContents = True
    , writerTOCDepth = 4
    , writerTemplate = Just template
    , writerVariables = latexVariables
    }
  where
    latexVariables :: [(String, String)]
    latexVariables = options

traverseRenderAll :: Item Pandoc -> Compiler (Item Pandoc)
traverseRenderAll =
  traverse
    (unsafeCompiler .
     walkM
       (renderAll $ RenderAllOptions {urlPrefix = Just "./", renderFormat = EPS}))
