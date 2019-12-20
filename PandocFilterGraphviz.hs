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

module PandocFilterGraphviz where

import           Control.Monad          (unless)
import           Crypto.Hash

import           Data.Byteable          (toBytes)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as C8

import qualified Data.Map.Strict        as M
import           Data.Text              as T
import           Data.Text.Encoding     as E

import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process         (readProcess, system)

import           Text.Pandoc
import           Text.Pandoc.JSON

(¤) :: Text -> Text -> Text
(¤) = T.append

hexSha3_512 :: ByteString -> ByteString
hexSha3_512 bs = C8.pack $ show (hash bs :: Digest SHA3_512)

sha :: Text -> Text
sha = E.decodeUtf8 . hexSha3_512 . B16.encode . E.encodeUtf8

fileName4Code :: Text -> Text -> Maybe Text -> FilePath
fileName4Code name source ext = filename
  where
    dirname = name ¤ "-images"
    shaN = sha source
    barename =
      shaN ¤
      (case ext of
         Just "msc" -> ".svg"
         Just "dot" -> ".svg"
         Just e     -> "." ¤ e
         Nothing    -> "")
    filename = T.unpack dirname </> T.unpack barename

getCaption :: M.Map Text Text -> (Text, Text)
getCaption m =
  case M.lookup "caption" m of
    Just cap -> (cap, "fig:")
    Nothing  -> ("", "")

ensureFile fp =
  let dir = takeDirectory fp
   in createDirectoryIfMissing True dir >> doesFileExist fp >>= \exist ->
        unless exist $ writeFile fp ""

renderDot :: String -> FilePath -> IO FilePath
renderDot src dst =
  readProcess "dot" ["-Tsvg", "-o" ++ dst] src >>
  return dst

-- Here is some msc rendering stuff
renderMsc :: String -> FilePath -> IO FilePath
renderMsc src dst =
  readProcess "mscgen" ["-Tsvg", "-o" ++ dst] src >>
  return dst

-- and we combine everything into one function
renderAll :: Block -> IO Block
renderAll cblock@(CodeBlock (id, classes, attrs) content)
  | "msc" `elem` classes =
    let dest = fileName4Code "mscgen" (T.pack content) (Just "msc")
     in do ensureFile dest
           img <- renderMsc content dest
           return $ image img
  | "graphviz" `elem` classes =
    let dest = fileName4Code "graphviz" (T.pack content) (Just "dot")
     in do ensureFile dest
           img <- renderDot content dest
           return $ image img
  | otherwise = return cblock
  where
    toTextPairs = Prelude.map (\(f, s) -> (T.pack f, T.pack s))
    m = M.fromList $ toTextPairs attrs
    (caption, typedef) = getCaption m
    image img =
      Para
        [ Image
            (id, classes, attrs)
            [Str $ T.unpack caption]
            ("/" </> img, T.unpack caption)
        ]
renderAll x = return x

stripHeading :: Block -> Block
stripHeading cblock@(Header level att content) = Null
stripHeading x                                 = x
