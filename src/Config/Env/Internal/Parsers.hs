{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Config.Env.Internal.Parsers
-- Description : Variables parsers and simple programs.
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
module Config.Env.Internal.Parsers (
    label,
    readVar,
    readVarOpt,
    readVarDef,
    stringVar,
    stringVarOpt,
    stringVarDef,
    parseVar,
    parseVarOpt,
    parseVarDef,
    varExists,
    jsonVar,
    jsonVarOpt,
    jsonVarDef
) where

    import           Config.Env.Internal.Config
    import qualified Data.Aeson                 as Aeson
    import qualified Data.ByteString.Lazy       as Lazy
    import           Data.String
    import qualified Data.Text.Lazy             as Text
    import qualified Data.Text.Lazy.Encoding    as Encoding
    import           Data.Typeable
    import           Text.Blaze.Html            (Html)
    import           Text.Read                  (readEither)

    -- | Label a subprogram.
    --
    -- This function causes a details node to encompass the documentation
    -- of the subprogram, with a given summary and possibly extra
    -- documentation.
    --
    -- This function only impacts generating documentation- when executing
    -- the program, it is a no-op.
    --
    label :: Html               -- The summary
                -> Maybe Html   -- Extra documentation
                -> Config a     -- The subprogram to wrap.
                -> Config a
    label = CLabel

    -- | Apply the read class to a parser.
    --
    -- Pulls out a common hunk of code.
    --
    reader :: forall a b . Read a =>
                (String -> (String -> Either String a) -> b)
                -> b
    reader p = p "read" readEither

    -- | Required variable parsed using the Read typeclass.
    readVar :: (Typeable a, Read a) => Variable -> Config a
    readVar = reader parseVar

    -- | Optional variable parsed using the Read typeclass.
    readVarOpt :: (Typeable a, Read a) => Variable -> Config (Maybe a)
    readVarOpt = reader parseVarOpt

    -- | Optional variable with a default parsed using the Read typeclass.
    readVarDef :: forall a . (Typeable a, Read a, Show a)
                    => a
                    -> Variable
                    -> Config a
    readVarDef = reader parseVarDef

    -- | Apply the IsString class to a parser.
    --
    -- Pulls out a common hunk of code.
    --
    stringer :: forall a b . IsString a =>
                (String -> (String -> Either String a) -> b)
                -> b
    stringer p = p "fromString" (Right . fromString)

    -- | Required variable parsed using the IsString typeclass.
    stringVar :: (Typeable a, IsString a) => Variable -> Config a
    stringVar = stringer parseVar

    -- | Optional variable parsed using the IsString typeclass.
    stringVarOpt :: (Typeable a, IsString a) => Variable -> Config (Maybe a)
    stringVarOpt = stringer parseVarOpt

    -- | Optional variable with a default parsed using the IsString typeclass.
    stringVarDef :: forall a . (Typeable a, IsString a, Show a)
                    => a
                    -> Variable
                    -> Config a
    stringVarDef = stringer parseVarDef

    -- | Apply the FromJSON class to a parser.
    --
    -- Pulls out a common hunk of code.
    --
    jsonifier :: forall a b . Aeson.FromJSON a =>
                (String -> (String -> Either String a) -> b)
                -> b
    jsonifier p = p "fromJSON" doDecode
        where
            doDecode :: String -> Either String a
            doDecode asString = 
                -- Haskell strings really do suck.
                let asText :: Text.Text
                    asText = Text.pack asString

                    asBytes :: Lazy.ByteString
                    asBytes = Encoding.encodeUtf8 asText

                in
                Aeson.eitherDecode' asBytes

    -- | Required variable parsed using the FromJSON typeclass.
    jsonVar :: (Typeable a, Aeson.FromJSON a) =>
                Variable
                -> Config a
    jsonVar = jsonifier parseVar

    -- | Optional variable parsed using the FromJSON typeclass.
    jsonVarOpt :: (Typeable a, Aeson.FromJSON a) =>
                    Variable
                    -> Config (Maybe a)
    jsonVarOpt = jsonifier parseVarOpt

    -- | Optional variable with a default parsed using the FromJSON typeclass.
    jsonVarDef :: (Typeable a, Show a, Aeson.FromJSON a) =>
                    a
                    -> Variable
                    -> Config a
    jsonVarDef = jsonifier parseVarDef

    -- | Required variable with a given parsing function.
    --
    -- This function fails if the variable does not exist, or if
    -- the parse function fails (returns Left).
    --
    parseVar :: Typeable a
                => String              
                    -- ^ The name of parser
                    --
                    -- The Read parsers use \"read\", the IsString
                    -- parsers \"fromString\", and FromJSON parser
                    -- use \"fromJSON\".  So use something else. 
                    -- But that gives you an idea.
                -> (String
                    -> Either String a)
                    -- ^ The parser function.
                    --
                    -- A return value of Left means an error occurred
                    -- with the given error message.  Otherwise, the
                    -- parsed value is returned.
                -> Variable             
                    -- ^ The variable information.
                -> Config a
    parseVar pType pFn eVar = CParse $ ParseInfo {
                                        parseType = pType,
                                        parseFn = pFn,
                                        envVar = eVar }

    -- | Optional variable with a given parsing function.
    --
    -- This function returns Nothing if the variable does not exist.
    -- It still fails if the parse function fails (returns Left).
    --
    parseVarOpt :: Typeable a
                    => String
                        -- ^ The name of parser
                        --
                        -- The Read parsers use \"read\", the IsString
                        -- parsers \"fromString\", and FromJSON parser
                        -- use \"fromJSON\".  So use something else. 
                        -- But that gives you an idea.

                    -> (String -> Either String a)
                        -- ^ The parser function.
                        --
                        -- A return value of Left means an error occurred
                        -- with the given error message.  Otherwise, the
                        -- parsed value is returned.

                    -> Variable
                        -- ^ The variable information.
                    -> Config (Maybe a)
    parseVarOpt pType pFn eVar = CParseOpt $ ParseInfo {
                                                parseType = pType,
                                                parseFn = pFn,
                                                envVar = eVar }

    -- | Optional variable with a default, with a given parsing function.
    --
    -- This function returns Nothing if the variable does not exist.
    -- It still fails if the parse function fails (returns Left).
    --
    parseVarDef :: forall a .  (Typeable a, Show a)
                    => String
                        -- ^ The name of parser
                        --
                        -- The Read parsers use \"read\", the IsString
                        -- parsers \"fromString\", and FromJSON parser
                        -- use \"fromJSON\".  So use something else. 
                        -- But that gives you an idea.

                    -> (String -> Either String a)
                        -- ^ The parser function.
                        --
                        -- A return value of Left means an error occurred
                        -- with the given error message.  Otherwise, the
                        -- parsed value is returned.

                    -> a
                        -- ^ The default value
                        --
                        -- Returned if the variable does not exist
                        -- (if the parse function fails, the program
                        -- still fails).
                        --
                        -- This type needs to implement the Show typeclass,
                        -- so that the documentation can show the
                        -- default value.

                    -> Variable
                        -- ^ The variable information.
                    -> Config a
    parseVarDef pType pFn d eVar = CParseDef d pinfo
        where
            pinfo :: ParseInfo a
            pinfo = ParseInfo {
                        parseType = pType,
                        parseFn = pFn,
                        envVar = eVar }

    -- | Test if a variable exists.
    --
    -- This program returns @True@ if the variable exists, or @False@ if
    -- it doesn't.
    --
    -- It is still possible for the program to fail if the low-level
    -- parser (such as the UTF8 decoding for file contents) fails.
    varExists :: Variable -> Config Bool
    varExists = parseVarDef "exists" (const (Right True)) False

