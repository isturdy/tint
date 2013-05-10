{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Text.Tint.Writers.Html5 (
    writeHtml5
  , writeHtml5Standalone
  ) where

import           Data.Monoid
import           Data.Text
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html5              (Html, docTypeHtml, toHtml, (!))
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes   hiding (content)
import           Text.Blaze.Internal           (stringValue, textValue)

import           Text.Tint.Colour
import           Text.Tint.Types

writeHtml5 :: CodeBlock -> Html
writeHtml5 code = H.pre ! class_ (textValue "tint") $ writeInner code
  where (CodeBlock {..}) = code

-- | Convert code to lazy text with integrated css, usable as a standalone
-- html file.
writeHtml5Standalone :: Colours -> CodeBlock -> LT.Text
writeHtml5Standalone colours code = renderHtml . docTypeHtml $
                                    H.body ! class_ (textValue "tint") $ do
  H.head . H.style $ toHtml (cssColours colours)
  H.body $ writeHtml5 code

writeInner :: CodeBlock -> Html
writeInner (CodeBlock {..}) = H.code ! class_ cls $ mapM_ writeSub content
  where cls = textValue $ pack "language-" <> language <> singleton ' '
                          <> T.unwords classes

writeSub :: SubBlock -> Html
writeSub (Line line) = H.span ! class_ (textValue "tint-line")
                       $ mapM_ writeCode line
writeSub (Sub  code) = writeInner code

writeCode :: Code -> Html
writeCode (Code NRM txt) = toHtml txt
writeCode (Code cls txt) = H.span ! class_ (stringValue $ show cls)
                           $ toHtml txt
