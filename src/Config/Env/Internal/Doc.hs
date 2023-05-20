{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Config.Env.Internal.Doc
-- Description : Generating the HTML documentation.
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
-- This module creates the Doc type, which represents a partially
-- generated documentation web page.
module Config.Env.Internal.Doc(
    Doc,
    labelDoc,
    parseVar,
    pureVal,
    failVal,
    andDoc,
    orDoc,
    selDoc,
    generateDoc,
    generateWebPage
) where

    import           Config.Env.Internal.Config
    import           Data.List.NonEmpty          (NonEmpty (..))
    import qualified Data.List.NonEmpty          as NonEmpty
    import           Data.Proxy
    import           Data.String                 (fromString)
    import           Data.Typeable
    import           Text.Blaze.Html             (Html)
    import qualified Text.Blaze.Html5            as Html
    import qualified Text.Blaze.Html5.Attributes as Attr

    -- | A partially-built HTML document.
    data Doc =
        DocLabel Html Html
        | DocVar Html Html
        | DocFail
        | DocPure
        | DocAnd (NonEmpty Labeled)
        | DocOr  (NonEmpty Labeled)
        | DocSel Html

    -- | A mostly-built HTML subdocument with an optional label.
    --
    -- Used for the children for DocAnd and DocOr.
    data Labeled = Labeled (Maybe Html) Html

    -- | Create a label around a document.
    --
    -- Corresponds to the `CLabel` constructor of Config.
    --
    labelDoc :: Html            -- ^ The summary
                -> Maybe Html   -- ^ Possible addition documentation
                -> Doc          -- ^ The document to wrap 
                -> Doc
    labelDoc _     _    DocFail = DocFail
    labelDoc label madd doc =
        let html1 :: Html
            html1 = generateDoc doc
            html2 :: Html
            html2 = case madd of
                        Nothing -> html1
                        Just x  -> x <> html1
        in DocLabel label html2

    -- | A parse variable node.
    --
    -- Used by the `CParse`, `CParseOpt`, and `CParserDef` constructors
    -- of Config.
    --
    -- Note that this generates a table of "field: value" pairs, so
    -- we allow adding extra fields on the end.  This lets us deal
    -- with default values, and possibly adding error messages, in
    -- a sane way.
    parseVar :: forall a .  Typeable a
                => Bool             -- ^ Is the variable required?
                                    --
                                    -- (False if the variable is optional)
                -> ParseInfo a      -- ^ Variable information
                -> [ (Html, Html) ] -- ^ Extra fields to add.
                -> Doc
    parseVar isReq pinfo xtras = DocVar htmlName $ genTable allRows
        where
            vari :: Variable
            vari = envVar pinfo

            varname :: VarName
            varname = name vari

            vname :: String
            vname = getVarName varname

            htmlName :: Html
            htmlName = Html.toHtml vname
            
            nameRow :: [ (Html, Html) ]
            nameRow = [ ("Name:", htmlName) ]

            requiredRow :: [ (Html, Html) ]
            requiredRow = [ ("Required:",
                                if isReq then "Required" else "Optional") ]

            descriptionRow :: [ (Html, Html) ]
            descriptionRow =
                case (description vari) of
                    Nothing -> []
                    Just desc -> [ ("Description:", desc) ]

            parseTypeRow :: [ (Html, Html) ]
            parseTypeRow = [ ("Parse Function:",
                                Html.toHtml (parseType pinfo)) ]

            typeRow :: [ (Html, Html) ]
            typeRow = [ ("Result Type:", 
                            fromString (show (typeRep (Proxy :: Proxy a)))) ]

            allRows :: [ (Html, Html) ]
            allRows = nameRow
                        ++ descriptionRow
                        ++ requiredRow
                        ++ parseTypeRow
                        ++ typeRow
                        ++ xtras

            genTable :: [ (Html, Html) ] -> Html
            genTable = (Html.table Html.! Attr.class_ "vartable")
                        . foldMap genRow 

            genRow :: (Html, Html) -> Html
            genRow (f1, f2) = Html.tr $ Html.td f1 <> Html.td f2

    -- Note: there is no mapping of CMap, as it has no effect on
    -- documentation.

    -- | A pure value node.
    --
    -- Used by the `CPure` constructor.
    --
    pureVal :: Doc
    pureVal = DocPure

    -- | A fail value node
    --
    -- Used by the `CEmpty` constructor.
    --
    failVal :: Doc
    failVal = DocFail

    -- | And two documents.
    --
    -- Used by the `CApp` constructor.
    --
    -- Note: we flatten multiple ands into one multi-way and.
    --
    andDoc :: Doc -> Doc -> Doc
    andDoc DocFail     _           = DocFail
    andDoc _           DocFail     = DocFail
    andDoc DocPure     x           = x
    andDoc x           DocPure     = x
    andDoc (DocAnd xs) (DocAnd ys) = DocAnd $ xs <> ys
    andDoc (DocAnd xs) y           = DocAnd $ snoc xs $ makeLabeled y
    andDoc x           (DocAnd ys) = DocAnd $ NonEmpty.cons (makeLabeled x) ys
    andDoc x           y           =
        DocAnd $ (makeLabeled x) :| [ makeLabeled y ]

    -- | Append an element to the end of a NonEmpty list.
    --
    -- Mildly annoyed NonEmpty doesn't already export this.
    --
    snoc :: NonEmpty x -> x -> NonEmpty x
    snoc (x :| xs) y = x :| (xs ++ [ y ])

    -- | Or two documents.
    --
    -- Used by the 'CAlt' constructor.
    --
    -- Note: we flatten multiple ors into one multi-way or.
    --
    orDoc :: Doc -> Doc -> Doc
    orDoc DocFail    y          = y
    orDoc x          DocFail    = x
    orDoc DocPure    _          = DocPure
    orDoc _          DocPure    = DocPure
    orDoc (DocOr xs) (DocOr ys) = DocOr (xs <> ys)
    orDoc (DocOr xs) y          = DocOr $ snoc xs $ makeLabeled y
    orDoc x          (DocOr ys) = DocOr $ NonEmpty.cons (makeLabeled x) ys
    orDoc x          y          =
        DocOr $ (makeLabeled x) :| [ makeLabeled y ]

    -- | The selective constructor.
    --
    -- Used by the `CSel` constructor.
    --
    selDoc :: Doc -> Doc -> Doc
    selDoc DocFail _ = DocFail
    selDoc x       y = 
        DocSel $
            generateLabeled (Just "Depending Upon") (makeLabeled x)
            <> generateLabeled (Just "Possibly")       (makeLabeled y)

    -- | Generate the "main body" of the documentation web page.
    --
    -- Note that you still need to call `generateWebPage` after
    -- calling this function to get a full web page.  This allows
    -- the addition of more content.
    generateDoc :: Doc -> Html
    generateDoc (DocLabel label body) = createDetails label body
    generateDoc (DocVar   _     body) = 
        Html.p "Variable:" <> body
    generateDoc DocFail               = Html.p "Always fails"
    generateDoc DocPure               = Html.p "A default value"
    generateDoc (DocAnd (x :| xs))    = 
        addRequirements $
            generateLabeled Nothing x
            <> foldMap (generateLabeled (Just "AND")) xs
    generateDoc (DocOr  (x :| xs))    =
        addRequirements $
            generateLabeled Nothing x
            <> foldMap (generateLabeled (Just "OR")) xs
    generateDoc (DocSel body)         = addRequirements body
    

    -- | Add the "Requirements: " Text.
    addRequirements :: Html -> Html
    addRequirements = mappend $ Html.p "Requirements:"

    -- | Convert a Doc to a Labeled.
    --
    -- This is why Doc isn't just Html.  We need to be able to \"promote\"
    -- a label up into it's encompassing Alt or App.
    --
    -- We don't want to just create an Html here, because we want to be
    -- able to modify the label depending upon where it lands.
    --
    makeLabeled :: Doc -> Labeled
    makeLabeled (DocLabel label body) = Labeled (Just label) body
    makeLabeled (DocVar   label body) = Labeled (Just label) body
    makeLabeled DocFail               = Labeled (Just "Constant Failure")
                                            (generateDoc DocFail)
    makeLabeled DocPure               = Labeled (Just "Constant Value")
                                            (generateDoc DocPure)
    makeLabeled x@(DocAnd _)          = Labeled Nothing (generateDoc x)   
    makeLabeled x@(DocOr  _)          = Labeled Nothing (generateDoc x)
    makeLabeled x@(DocSel _)          = Labeled Nothing (generateDoc x)

    -- Convert a Labeled subdocument to a full Html.
    generateLabeled :: (Maybe Html) -- ^ What to prefix to the label,
                                    -- if anything.
                        -> Labeled
                        -> Html
    generateLabeled prefix (Labeled label body) =
            createDetails (createSummary prefix label) body
        where
            createSummary :: Maybe Html -> Maybe Html -> Html
            createSummary (Just pfix) (Just desc) =
                pfix <> " " <> desc
            createSummary (Just pfix) Nothing = pfix <> " Configuration"
            createSummary Nothing (Just desc) = desc
            createSummary Nothing Nothing = "Configuration"

    -- | Create a details tag around some body
    createDetails :: Html       -- ^ The summary
                        -> Html -- ^ The body
                        -> Html
    createDetails summ body =
        Html.details $ do
            Html.summary summ
            Html.div Html.! Attr.class_ "indent" $ body

    -- | Generate a full web page from the given body.
    --
    -- This adds the header (with the necessary styles), and the
    -- wrapper stuff (html and body tags, etc.).
    generateWebPage :: Html         -- | The title of the webpage
                        -> Html     -- | The body
                        -> Html
    generateWebPage title doc = Html.docTypeHtml $ head_ <> body
        where
            head_ :: Html
            head_ = Html.head $
                    meta1
                    <> meta2
                    <> Html.title title
                    <> Html.style (Html.preEscapedToHtml style)

            meta1 :: Html
            meta1 = Html.meta Html.! Attr.charset "UTF-8"

            meta2 :: Html
            meta2 = Html.meta
                        Html.! Attr.name "viewport"
                        Html.! Attr.content 
                            "width=device-width, initial-scale=1.0"

            style :: String
            style = ".variable { border: none; }\
                        \ .indent { padding-left: 2em; }\
                        \ .example {\
                            \ background-color: #e0ffe0;\
                            \ width: 98%;\
                            \ padding: 0.25em;\
                            \ font-size: 18px;\
                        \ }"
                                  
            body :: Html
            body = Html.body doc

