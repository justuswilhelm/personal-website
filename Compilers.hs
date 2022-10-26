-- Contain compilers used in this site
module Compilers
  ( customPostPandocCompiler
  , customTeaserPandocCompiler
  , replaceTocExtension
  , writePandocLatexWith
  , relativizeUrlsWithCompiler
  , traverseRenderAll
  ) where

import Data.ByteString.Lazy (ByteString)

import qualified Data.Map as M
import qualified Data.Text as T
import Hakyll
  ( Compiler
  , ContextField(..)
  , Item(..)
  , defaultHakyllReaderOptions
  , defaultHakyllWriterOptions
  , makeItem
  , pandocCompilerWithTransform
  , pandocCompilerWithTransformM
  , replaceAll
  , unContext
  , unsafeCompiler
  , unsafeCompiler
  , withUrls
  , withUrls
  )
import Text.DocTemplates
  ( Context(..)
  , Template
  , Val(..)
  , compileTemplate
  , compileTemplateFile
  , toVal
  )
import Text.Pandoc
  ( HTMLMathMethod(MathJax)
  , Pandoc
  , WriterOptions(..)
  , pandocExtensions
  , runIO
  , runPure
  , runWithDefaultPartials
  , writeLaTeX
  )
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.Walk (walk, walkM)

import PandocFilterGraphviz
  ( RenderAllOptions(..)
  , RenderFormat(..)
  , relativizePandocUrls
  , renderAll
  , stripHeading
  )

import Contexts (postCtx)

makePostHakyllWriterOptions :: Template T.Text -> WriterOptions
makePostHakyllWriterOptions template =
  defaultHakyllWriterOptions
    { writerSectionDivs = True
    , writerTableOfContents = True
    , writerTemplate = Just template
    , writerTOCDepth = 4
    , writerHtmlQTags = True
    , writerExtensions = pandocExtensions
    , writerHTMLMathMethod = MathJax $ T.pack ""
    }

customPostPandocCompiler :: Compiler (Item String)
customPostPandocCompiler = do
  let template =
        either error id $
        either (error . show) id $
        runPure $ runWithDefaultPartials $compileTemplate "toc" templateText
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    (makePostHakyllWriterOptions template)
    (unsafeCompiler . walkM (renderAll fmt))
  where
    templateText =
      T.pack
        "<div id=\"TOC\">$toc$</div>\n<div id=\"markdownBody\">$body$</div>"
    fmt =
      RenderAllOptions
        {urlPrefix = Nothing, renderFormat = SVG, embedRendered = True}

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
  let variables =
        [ (T.pack "author", toVal author)
        , (T.pack "title", toVal title)
        , (T.pack "date", toVal date)
        ]
  pdfString <-
    unsafeCompiler $ do
      compiledTemplate <- compileTemplateFile templatePath
      case compiledTemplate of
        Left err -> makeErrorString err title
        Right template -> do
          let options = pdfHakyllWriterOptions variables template
          pdfOrError <- runIO $ makePDF "xelatex" [] writeLaTeX options doc
          -- Homework: Find out why this is a nested Either
          case pdfOrError of
            Left err -> makeErrorString err title
            Right success ->
              case success of
                Left err -> makeErrorString err title
                Right pdf -> return pdf
  makeItem pdfString
  where
    makeErrorString err title =
      error $ "Main.writePandocLatexWith: " ++ show err ++ T.unpack title
    getString (StringField s) = return $ T.pack s
    getString (ListField _ _) = return $ T.pack ""
    getString EmptyField = error "wtf"
    getStringFromContext s = Hakyll.unContext postCtx s [] item >>= getString
    templatePath = "templates/post.tex"

relativizeUrlsWithCompiler :: String -> Item Pandoc -> Compiler (Item Pandoc)
relativizeUrlsWithCompiler _ item =
  return $ fmap (walk $ relativizePandocUrls $ T.pack ".") item

pdfHakyllWriterOptions ::
     [(T.Text, Val T.Text)] -> Template T.Text -> WriterOptions
pdfHakyllWriterOptions options template =
  defaultHakyllWriterOptions
    { writerSectionDivs = True
    , writerTableOfContents = True
    , writerTOCDepth = 4
    , writerTemplate = Just template
    -- is this some typeclass magic? XXX
    , writerVariables = Context $ M.fromList latexVariables
    }
  where
    latexVariables :: [(T.Text, Val T.Text)]
    latexVariables = options

traverseRenderAll :: Item Pandoc -> Compiler (Item Pandoc)
traverseRenderAll =
  traverse (unsafeCompiler . walkM (renderAll renderAllOptions))
  where
    renderAllOptions =
      RenderAllOptions
        {urlPrefix = Just "./", renderFormat = EPS, embedRendered = False}
