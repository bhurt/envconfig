{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Config.Env.Internal.Examples
-- Description : Creating HTML examples.
-- Copyright   : (c) Brian Hurt, 2023
-- License     : BSD 3-Clause
-- Maintainer  : Brian Hurt <bhurt42@gmail.com>
-- Stability   : experimental
--
-- This is an internal module of the envconfig package.  You almost
-- certainly want to import "Config.Env" instead.  Anything exported
-- by this module but not re-exported by "Config.Env" is subject to
-- change without notice.
--
module Config.Env.Internal.Examples (
    example,
    jsonExample
) where

    import qualified Data.Aeson                  as Aeson
    import qualified Data.Aeson.Encode.Pretty    as Pretty
    import           Data.String                 (fromString)
    import           Data.Text                   (Text)
    import qualified Data.Text.Lazy.Builder      as Lazy
    import           Text.Blaze.Html             (Html)
    import           Text.Blaze.Html5            as Html
    import           Text.Blaze.Html5.Attributes as Attr

    -- | An HTML newline.
    newline :: Html
    newline = Html.toHtml ("\n" :: String)

    -- | A wrapper around a details/summary tag pair.
    collapse :: String -> Html -> Html
    collapse summary inner =
        Html.details $ do
            Html.summary (fromString summary)
            Html.div ! Attr.class_ "indent" $ do
                Html.pre ! Attr.class_ "example" $ do
                    newline
                    inner
                    newline
                

    -- | Create an example in HTML from literal text.
    --
    -- Useful for documention.  The text is included literally (without
    -- escaping), wrapped in an HTML PRE tag, wrapped in a
    -- details/summary tag pair.
    example :: String -> Html
    example txt = collapse "Example" $ Html.preEscapedString txt

    -- | Create an example in HTML of a JSON encoding of an object.
    --
    -- Useful for documents parseJSON* variables, this lets you
    -- include an example JSON document.
    jsonExample :: forall a . Aeson.ToJSON a => a -> Html
    jsonExample a = collapse "JSON Example" $ ex
        where
            ex :: Html
            ex = Html.toHtml bs

            bs :: Lazy.Builder
            bs = Pretty.encodePrettyToTextBuilder a

