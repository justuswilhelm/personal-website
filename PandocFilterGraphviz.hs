--- Copyright (c) 2017, Jean-Pierre PRUNARET
---
--- All rights reserved.
---
--- Redistribution and use in source and binary forms, with or without
--- modification, are permitted provided that the following conditions are met:
---
---     * Redistributions of source code must retain the above copyright
---       notice, this list of conditions and the following disclaimer.
---
---     * Redistributions in binary form must reproduce the above
---       copyright notice, this list of conditions and the following
---       disclaimer in the documentation and/or other materials provided
---       with the distribution.
---
---     * Neither the name of Jean-Pierre PRUNARET nor the names of other
---       contributors may be used to endorse or promote products derived
---       from this software without specific prior written permission.
---
--- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
--- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
--- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
--- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
--- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
--- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
{-# LANGUAGE OverloadedStrings #-}

module PandocFilterGraphviz
  ( renderAll
  , stripHeading
  , RenderFormat(..)
  , RenderAllOptions(..)
  , relativizePandocUrls
  ) where

import Crypto.Hash

import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import qualified Data.Map.Strict as M

import System.Directory
import System.FilePath
import System.Process (readProcess)

import Text.Pandoc

hexSha3_512 :: ByteString -> String
hexSha3_512 bs = show (hash bs :: Digest SHA3_512)

sha :: String -> String
sha = hexSha3_512 . B16.encode . C8.pack

data RenderFormat
  = SVG
  | EPS
  deriving (Show)

fileName4Code :: RenderFormat -> RenderTool -> T.Text -> FilePath
fileName4Code format toolName source = filename
  where
    dirname = show toolName ++ "-images"
    shaN = sha $ T.unpack source
    extension =
      case format of
        SVG -> ".svg"
        EPS -> ".eps"
    barename = shaN ++ extension
    filename = dirname </> barename

ensureFileWriteable :: FilePath -> IO ()
ensureFileWriteable fp = createDirectoryIfMissing True $ takeDirectory fp

formatToFlag :: RenderFormat -> String
formatToFlag format =
  case format of
    SVG -> "svg"
    EPS -> "eps"

data RenderTool
  = Mscgen
  | Graphviz
  deriving (Show)

renderToolToCmd :: RenderTool -> String
renderToolToCmd tool =
  case tool of
    Mscgen -> "mscgen"
    Graphviz -> "dot"

renderDotLike :: RenderTool -> RenderFormat -> T.Text -> FilePath -> IO FilePath
renderDotLike renderTool format src dst = do
  ensureFileWriteable dst
  _ <- readProcess cmd ["-T", formatToFlag format, "-o", dst] $ T.unpack src
  return dst
  where
    cmd = renderToolToCmd renderTool

renderDotLikeEmbed :: RenderTool -> RenderFormat -> String -> IO String
renderDotLikeEmbed renderTool format =
  readProcess cmd $ ["-T", formatToFlag format] ++ outputFlag
  where
    cmd = renderToolToCmd renderTool
    outputFlag =
      case renderTool
         -- Mscgen is unhappy when -o is not passed
            of
        Mscgen -> ["-o", "-"]
        Graphviz -> []

-- and we combine everything into one function
data RenderAllOptions =
  RenderAllOptions
    { urlPrefix :: Maybe String
    , renderFormat :: RenderFormat
    , embedRendered :: Bool
    }
  deriving (Show)

renderAll :: RenderAllOptions -> Block -> IO Block
renderAll options cblock@(CodeBlock (blockId, classes, attrs) content)
  | "msc" `elem` classes = do
    let dest = destForTool Mscgen
    destpath <- renderDotLike Mscgen format content dest
    source <- renderDotLikeEmbed Mscgen format $ T.unpack content
    return $
      if embed
        then embedImage $ T.pack source
        else image $ T.pack destpath
  | "graphviz" `elem` classes = do
    let dest = destForTool Graphviz
    destpath <- renderDotLike Graphviz format content dest
    source <- renderDotLikeEmbed Graphviz format $ T.unpack content
    return $
      if embed
        then embedImage $ T.pack source
        else image $ T.pack destpath
  | otherwise = return cblock
  where
    embed = embedRendered options
    format = renderFormat options
    destForTool toolName = fileName4Code format toolName content
    m = M.fromList attrs
    caption = fromMaybe "" (getCaption m)
    getCaption = M.lookup "caption"
    image :: T.Text -> Block
    image img =
      Para
        [ Image
            (blockId, classes, attrs)
            [Str caption]
            ( case urlPrefix options of
                Just prefix -> T.pack $ prefix </> T.unpack img
                Nothing -> img
            , caption)
        ]
    embedImage = RawBlock (Format "html")
renderAll _ x = return x

relativizePandocUrls :: T.Text -> Inline -> Inline
relativizePandocUrls with (Image a b (url, title)) =
  Image a b (T.append with url, title)
relativizePandocUrls _ x = x

stripHeading :: Block -> Block
stripHeading Header {} = Null
stripHeading x = x
