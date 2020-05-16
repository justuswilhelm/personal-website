-- Contain compilers used in this site
module Compilers
  ( customPostPandocCompiler
  , customTeaserPandocCompiler
  , replaceTocExtension
  ) where

import           Hakyll               (Compiler, Item,
                                       defaultHakyllReaderOptions,
                                       defaultHakyllWriterOptions,
                                       pandocCompilerWithTransform,
                                       pandocCompilerWithTransformM, replaceAll,
                                       unsafeCompiler, withUrls)
import           Text.Pandoc          (Format (..), Pandoc, WriterOptions (..))
import           Text.Pandoc.Walk     (walk, walkM)

import           PandocFilterGraphviz (RenderAllOptions (..),
                                       RenderFormat (SVG), renderAll,
                                       stripHeading)

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
