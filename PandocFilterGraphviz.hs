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
  ) where

import           Control.Arrow          ((***))
import           Control.Monad          (join, unless)
import           Crypto.Hash

import           Data.Byteable          (toBytes)
import qualified Data.ByteString.Base16 as B16
import           Data.ByteString.Char8  (ByteString)
import qualified Data.ByteString.Char8  as C8
import           Data.Maybe             (fromMaybe)

import qualified Data.Map.Strict        as M
import           Data.Text.Encoding     as E

import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process         (readProcess, system)

import           Text.Pandoc
import           Text.Pandoc.JSON

hexSha3_512 :: ByteString -> String
hexSha3_512 bs = show (hash bs :: Digest SHA3_512)

sha :: String -> String
sha = hexSha3_512 . B16.encode . C8.pack

data RenderFormat =
  SVG
  deriving (Show)

fileName4Code :: RenderFormat -> String -> String -> FilePath
fileName4Code format toolName source = filename
  where
    dirname = toolName ++ "-images"
    shaN = sha source
    extension =
      case format of
        SVG -> ".svg"
    barename = shaN ++ extension
    filename = dirname </> barename

ensureFileWriteable :: FilePath -> IO ()
ensureFileWriteable fp = createDirectoryIfMissing True $ takeDirectory fp

formatToFlag :: RenderFormat -> String
formatToFlag format =
  case format of
    SVG -> "svg"

renderDotLike :: String -> RenderFormat -> FilePath -> String -> IO ()
renderDotLike cmd format dst src = do
  ensureFileWriteable dst
  readProcess cmd ["-T", formatToFlag format, "-o", dst] src
  return ()

renderDot :: RenderFormat -> FilePath -> String -> IO ()
renderDot = renderDotLike "dot"

-- Here is some msc rendering stuff
renderMsc :: RenderFormat -> FilePath -> String -> IO ()
renderMsc = renderDotLike "mscgen"

-- and we combine everything into one function
data RenderAllOptions = RenderAllOptions
  { renderFormat :: RenderFormat
  } deriving (Show)

renderAll :: RenderAllOptions -> Block -> IO Block
renderAll options cblock@(CodeBlock (id, classes, attrs) content)
  | "msc" `elem` classes = do
    let dest = destForTool "mscgen"
    renderMsc format dest content
    return $ image dest
  | "graphviz" `elem` classes = do
    let dest = destForTool "graphviz"
    renderDot format dest content
    return $ image dest
  | otherwise = return cblock
  where
    format = renderFormat options
    destForTool toolName = fileName4Code format toolName content
    m = M.fromList attrs
    caption = fromMaybe "" (getCaption m)
    getCaption = M.lookup "caption"
    image img =
      Para [Image (id, classes, attrs) [Str caption] ("/" </> img, caption)]
renderAll pre x = return x

stripHeading :: Block -> Block
stripHeading cblock@(Header level att content) = Null
stripHeading x                                 = x
