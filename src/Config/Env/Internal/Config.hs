{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs         #-}

-- |
-- Module      : Config.Env.Internal.Config
-- Description : The Config type and associates
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
-- This module defines the main Config type, and it's required types.
--
module Config.Env.Internal.Config (
    VarName,
    getVarName,
    Variable(..),
    ParseInfo(..),
    Config(..)
) where

    import           Control.Applicative
    import           Control.Selective
    import           Data.Char           (isAlphaNum)
    import           Data.String         (IsString, fromString)
    import           Data.Typeable
    import           GHC.Generics
    import           Text.Blaze.Html     (Html)

    -- | A variable name that only contains valid characters.
    --
    -- Note that we limit what characters can be in a variable name
    -- for sanity reasons.  Allowable characters are alphanumeric
    -- characters, the period (.) and the underscore (_).  A VarName
    -- is then a newtype wrapper around a string that signals it only
    -- contains valid characters
    --
    -- We do not supply a read instance as this would undercut the
    -- guarantee.  We do supply an IsString instance, so that by
    -- turning on the OverloadedStrings language extension, you can
    -- use (valid) raw strings as var names.
    newtype VarName = VarName { 
            -- | Unwrap a VarName to get the underlying string.
            getVarName :: String }
        deriving (Show, Ord, Eq, Typeable, Generic)

    -- | Convert a string to a `VarName`.
    --
    -- Checks that only allowable characters are in the name.
    var :: String -> VarName
    var s = 
            case all goodChar s of
                True -> VarName s
                False -> error $ "Invalid characters in variable name "
                                    ++ show s
        where
            goodChar :: Char -> Bool
            goodChar c = isAlphaNum c || (c == '.') || (c == '_')

    instance IsString VarName where
        fromString = var

    -- | A Variable.
    --
    -- This structure wraps both the variable name and an optional
    -- description.  The description is used to generate the documentation.
    -- Note that Variable implements IsString, which means that with
    -- the OverloadedStrings language instruction, you can use constant
    -- strings to generate variables with no description.  Alternatively,
    -- you can do:
    --
    -- @
    --      "var_name" { description = Just $ "Some documentation" }
    -- @
    --
    -- Unfortunately, the limitations of the `Html` type means we can't
    -- implement the normal gang of typeclasses (Eq, Ord, Show) we
    -- might want to.
    data Variable = Variable {
                    name :: VarName,
                    description :: Maybe Html }
        deriving (Typeable, Generic)

    instance IsString Variable where
        fromString s = Variable {
                            name = var s,
                            description = Nothing }


    -- | Information about a parse node.
    --
    -- Rather than have each of the various parse nodes of the `Config`
    -- type take a bunch of arguments, I bind them up into a record.
    -- This cleans up the code.
    data ParseInfo a = ParseInfo {
                        -- | The name of the parse type.
                        --
                        -- For example, "read", "fromString", "fromJSON",
                        -- etc.
                        parseType :: String,

                        -- | The actual parse function.
                        --
                        -- For example, read, fromString, etc.
                        parseFn :: String -> Either String a,

                        -- | The passed-in variable information.
                        envVar :: Variable }

    -- | A program that creates a configuration value.
    --
    data Config a where
        -- The reason we use a GADT here is to let us capture some
        -- typeclass constraints.


        -- | A label node.
        --
        -- Label nodes wrap a Config with a description tag, and optionally
        -- some additional documentation.  They are used to block out a
        -- subprogram as a logically coherent block.  Note, this only
        -- impacts the documentation generated, for normal execution
        -- this is a no-op.
        CLabel     :: Html              -- ^ The contents of the summary tag.
                        -> Maybe Html   -- ^ The optional additional
                                        -- documentation.
                                        --
                                        -- The will be prepended before the
                                        -- documentation of the subprogram.
                        -> Config a     -- ^ The subprogram to wrap.
                        -> Config a

        -- | Parse a variable.
        --
        -- If the variable does not exist, fail.  If the variable can
        -- not be parsed, fail.
        CParse     :: Typeable a
                        => ParseInfo a -- ^ parse info
                        -> Config a

        -- | Parse an optional variable.
        --
        -- If the variable does not exist, return Nothing.  If the variable
        -- can not be parsed, fail.
        --
        CParseOpt  :: Typeable a
                        => ParseInfo a      -- ^ parse info
                        -> Config (Maybe a)

        -- | Parse an optional variable with a default.
        --
        -- If the variable does not exist, return the default value.
        -- If the variable can not be parsed, fail.
        --
        -- The reason this exists, rather than just doing the obvious
        -- fmap over CParseOpt, is so that the documentation can show
        -- the default value.  This, of course, requires a Show
        -- constraint
        CParseDef  :: (Typeable a, Show a)
                        => a                -- ^ default value
                        -> ParseInfo a      -- ^ parse info
                        -> Config a

        -- | Map the result value of a Config.
        --
        -- If the source config fails, the result config fails.
        -- Implements fmap.
        CMap       :: (a -> b)      -- ^ Conversion function
                        -> Config a -- ^ Source
                        -> Config b

        -- | A program that just returns a constant value.
        --
        -- This never fails.
        --
        -- Used to implement Applicative's pure function.
        CPure      :: a             -- ^ Value to return
                        -> Config a

        -- | Applicative functor
        --
        -- If either subprogram fails, this program fails.
        --
        -- Use to implement Applicative's <$> operator.
        --
        -- If CAlt is logical (exclusive-)or, CApp is logical and.
        CApp       :: Config (a -> b)   -- ^ Source of the constructor
                        -> Config a     -- ^ Source of the value
                        -> Config b

        -- | A program that always fails.
        --
        -- Used to implement Alternative's empty.
        CEmpty     :: Config a      -- ^ Always fails.

        -- | Alternative functor.
        --
        -- If the first program succeeds, it's value is used and the second
        -- program is never executed.  If the first program fails, only
        -- then is the second program executed.  If the second program
        -- also fails, then this program fails.  Otherwise, if the
        -- first program fails and the second program succeeds, the
        -- second program's result is used.
        --
        -- If CAlt is logical (exclusive-)or, CApp is logical and.
        CAlt       :: Config a      -- ^ First choice
                        -> Config a -- ^ Second choice
                        -> Config a

        -- | Selective functor
        --
        -- The first program is always executed.  If it fails, this program
        -- fails.  If it succeeds, then the value it produces is used to
        -- determine if the second program executes.  If the second program
        -- is executed and fails, this program fails.  If the second program
        -- is executed and succeeds, it's result becomes the result of
        -- the program.
        CSel       :: Config (Either a b)   -- ^ The choice maker
                        -> Config (a -> b)  -- ^ The optional program
                        -> Config b

    instance Functor Config where
        fmap f (CMap g c) = CMap (f . g) c
        fmap _ CEmpty     = CEmpty
        fmap f (CPure x)  = CPure (f x)
        fmap f c          = CMap f c

    instance Applicative Config where
        pure = CPure
        CEmpty  <*> _       = CEmpty
        _       <*> CEmpty  = CEmpty
        CPure f <*> x       = fmap f x
        f       <*> CPure x = fmap ($ x) f
        x       <*> y       = CApp x y

    instance Alternative Config where
        empty = CEmpty
        CEmpty      <|> y      = y
        x           <|> CEmpty = x
        x@(CPure _) <|> _      = x
        x           <|> y      = CAlt x y

    instance Selective Config where
        select CEmpty    _ = CEmpty
        select (CPure x) y =
            case x of
                Left a  -> fmap ($ a) y
                Right b -> CPure b
        select x         y = CSel x y

    instance Semigroup a => Semigroup (Config a) where
        x <> y = (<>) <$> x <*> y

    instance Monoid a => Monoid (Config a) where
        mempty = pure mempty
        mappend = (<>)


