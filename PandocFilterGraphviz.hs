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

import Crypto.Hash
import Control.Monad (unless)

import Data.ByteString (ByteString)
import Data.Byteable (toBytes)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as B16

import qualified Data.Map.Strict as M
import Data.Text as T
import Data.Text.Encoding as E

import System.FilePath
import System.Directory
import System.Exit
import System.Process (system)

import Text.Pandoc
import Text.Pandoc.JSON

(¤) :: Text -> Text -> Text
(¤) = T.append

hexSha3_512 :: ByteString -> ByteString
hexSha3_512 bs = C8.pack $ show (hash bs :: Digest SHA3_512)

sha :: Text -> Text
sha = E.decodeUtf8 . hexSha3_512 . B16.encode . E.encodeUtf8

fileName4Code :: Text -> Text -> Maybe Text -> FilePath
fileName4Code name source ext =
    filename
    where
        dirname = name ¤ "-images"
        shaN = sha source
        barename = shaN ¤ (case ext of
            Just e -> "." ¤ e
            Nothing -> "")
        filename = T.unpack dirname </> T.unpack barename

getCaption :: M.Map Text Text -> (Text,Text)
getCaption m = case M.lookup "caption" m of
    Just cap -> (cap,"fig:")
    Nothing -> ("","")

renderDot1 :: FilePath -> IO FilePath
renderDot1 src =
    renderDot src dst >> return dst
    where
    dst = (dropExtension src) <.> "svg"

renderDot :: FilePath -> FilePath -> IO ExitCode
renderDot src dst =
  system $
    Prelude.unwords [ "dot"
                    , "-Tsvg"
                    , "-o" ++ show dst
                    , show src ]

graphviz :: Block -> IO Block
graphviz cblock@(CodeBlock (id, classes, attrs) content) =
    if "graphviz" `elem` classes then do
        ensureFile dest >> writeFile dest content
        img <- renderDot1 dest
        ensureFile img
        return $ Para [Image (id,classes,attrs) [Str $ T.unpack caption] ("/" </> img, T.unpack caption)]
    else return cblock
    where
        dest = fileName4Code "graphviz" (T.pack content) (Just "dot")
        ensureFile fp =
            let dir = takeDirectory fp in
            createDirectoryIfMissing True dir >> doesFileExist fp >>=
                \exist ->
                unless exist $ writeFile fp ""
        toTextPairs = Prelude.map (\(f,s) -> (T.pack f,T.pack s))
        m = M.fromList $ toTextPairs $ attrs
        (caption, typedef) = getCaption m
graphviz x = return x

-- Here is some msc rendering stuff

renderMsc1 :: FilePath -> IO FilePath
renderMsc1 src =
    renderMsc src dst >> return dst
    where
    dst = (dropExtension src) <.> "svg"

renderMsc :: FilePath -> FilePath -> IO ExitCode
renderMsc src dst =
  system $
    Prelude.unwords [ "mscgen"
                    , "-Tsvg"
                    , "-o" ++ show dst
                    , show src ]

msc :: Block -> IO Block
msc cblock@(CodeBlock (id, classes, attrs) content) =
  if "msc" `elem` classes then do
    ensureFile dest >> writeFile dest content
    img <- renderMsc1 dest
    ensureFile img
    return $ Para [Image (id,classes,attrs) [Str $ T.unpack caption] ("/" </> img, T.unpack caption)]
  else return cblock
  where
    dest = fileName4Code "mscgen" (T.pack content) (Just "msc")
    ensureFile fp =
      let dir = takeDirectory fp in
      createDirectoryIfMissing True dir >> doesFileExist fp >>=
        \exist ->
          unless exist $ writeFile fp ""
    toTextPairs = Prelude.map (\(f,s) -> (T.pack f,T.pack s))
    m = M.fromList $ toTextPairs $ attrs
    (caption, typedef) = getCaption m
msc x = return x
