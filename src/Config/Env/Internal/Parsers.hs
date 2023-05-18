{-# LANGUAGE ScopedTypeVariables #-}

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
    varExists
) where

    import           Config.Env.Internal.Config
    import           Data.String
    import           Data.Typeable
    import           Text.Blaze.Html            (Html)
    import           Text.Read                  (readEither)


    label :: Html -> Maybe Html -> Config a -> Config a
    label = CLabel

    reader :: forall a b . Read a =>
                (String -> (String -> Either String a) -> b)
                -> b
    reader p = p "read" readEither

    readVar :: (Typeable a, Read a) => Variable -> Config a
    readVar = reader parseVar

    readVarOpt :: (Typeable a, Read a) => Variable -> Config (Maybe a)
    readVarOpt = reader parseVarOpt

    readVarDef :: forall a . (Typeable a, Read a, Show a)
                    => a
                    -> Variable
                    -> Config a
    readVarDef = reader parseVarDef

    stringer :: forall a b . IsString a =>
                (String -> (String -> Either String a) -> b)
                -> b
    stringer p = p "fromString" (Right . fromString)

    stringVar :: (Typeable a, IsString a) => Variable -> Config a
    stringVar = stringer parseVar

    stringVarOpt :: (Typeable a, IsString a) => Variable -> Config (Maybe a)
    stringVarOpt = stringer parseVarOpt

    stringVarDef :: forall a . (Typeable a, IsString a, Show a)
                    => a
                    -> Variable
                    -> Config a
    stringVarDef = stringer parseVarDef

    parseVar :: Typeable a => String
                                -> (String -> Either String a)
                                -> Variable
                                -> Config a
    parseVar pType pFn eVar = CParse $ ParseInfo {
                                        parseType = pType,
                                        parseFn = pFn,
                                        envVar = eVar }

    parseVarOpt :: Typeable a => String
                                    -> (String -> Either String a)
                                    -> Variable
                                    -> Config (Maybe a)
    parseVarOpt pType pFn eVar = CParseOpt $ ParseInfo {
                                                parseType = pType,
                                                parseFn = pFn,
                                                envVar = eVar }

    parseVarDef :: forall a .  (Typeable a, Show a)
                        => String
                        -> (String -> Either String a)
                        -> a
                        -> Variable
                        -> Config a
    parseVarDef pType pFn d eVar = CParseDef d pinfo
        where
            pinfo :: ParseInfo a
            pinfo = ParseInfo {
                        parseType = pType,
                        parseFn = pFn,
                        envVar = eVar }

    varExists :: Variable -> Config Bool
    varExists = parseVarDef "exists" (const (Right True)) False

