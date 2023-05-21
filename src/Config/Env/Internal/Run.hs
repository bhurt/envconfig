{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Config.Env.Internal.Run
-- Description : Running Config programs
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
module Config.Env.Internal.Run (
    ReadResult(..),
    runConfigBase,
    runConfig,
    runConfigDirBase,
    runConfigDir,
    helpConfig
) where

    import           Config.Env.Internal.Config
    import           Config.Env.Internal.Doc
    import qualified Data.ByteString.Lazy       as Lazy
    import           Data.String                (fromString)
    import qualified Data.Text.Lazy             as LazyText
    import qualified Data.Text.Lazy.Encoding    as Encoding
    import qualified System.Directory           as Dir
    import           System.Environment         (lookupEnv)
    import           System.FilePath            ((</>))
    import           Text.Blaze.Html            (Html)

    -- | The results of a read.
    --
    -- This was original a combination of Maybe and Either, which is
    -- a sure sign that a more specific data structure is called for.
    data ReadResult a =
        NotFound            -- ^ Variable does not exists
        | ReadError String  -- ^ There was an error reading the variable
                            --
                            -- For example, if UTF-8 decoding fails.
                            --
                            -- This is the dreaded \"low-level\" error
                            -- referenced elsewhere.
        | Contents a        -- ^ Variable exists and can be read.
                            --
                            -- The contents of the variable are returned
                            -- as a string.

    -- | Executing a Config program with a given variable reader.
    --
    -- This function is exported to allow different sources of
    -- configuration information.  Pass in the right function, and
    -- you're reading the configuration variables from a database,
    -- or an Amazon S3 bucket, or where ever.
    --
    -- The Html generated on error still needs to be passed to
    -- `generateWebPage`.  This is done so that the caller and add
    -- additional content before or after the main tree.
    runConfigBase ::
        forall a . 
        (VarName -> IO (ReadResult String)) -- ^ The variable reader
        -> Config a                         -- ^ The config to execute
        -> IO (Either Html a)
    runConfigBase reader config = do
            r <- go config
            pure $
                case r of
                    Left doc  -> Left $ generateDoc doc
                    Right res -> Right res
        where
            go :: forall x . Config x -> IO (Either Doc x)
            go (CLabel lbl desc cfg) = lmap (labelDoc lbl desc) <$> go cfg
            go (CParse pinfo) =
                let makeErr :: String -> Either Doc x
                    makeErr err = Left $
                                    parseVar True pinfo
                                        [ ("Error:", fromString err) ]

                    fixup :: ReadResult x -> Either Doc x
                    fixup NotFound        = makeErr "Variable not found."
                    fixup (ReadError err) = makeErr err
                    fixup (Contents x)    = Right x
                in
                fixup <$> readvar pinfo
            go (CParseOpt pinfo) =
                let fixup :: forall y . ReadResult y -> Either Doc (Maybe y)
                    fixup NotFound        = Right Nothing
                    fixup (ReadError err) =
                        Left $ parseVar False pinfo
                                [ ("Error:", fromString err) ]
                    fixup (Contents x)    = Right $ Just x
                in
                fixup <$> readvar pinfo
            go (CParseDef def pinfo) =
                let fixup :: ReadResult x -> Either Doc x
                    fixup NotFound        = Right def
                    fixup (ReadError err) =
                        Left $ parseVar False pinfo
                                [   ("Default:", fromString (show def)),
                                    ("Error:", fromString err) ]
                    fixup (Contents x)    = Right x
                in
                fixup <$> readvar pinfo
            
            go (CMap f cfg) = fmap f <$> go cfg
            go (CPure x) = pure $ Right x
            go (CApp fn xn) = do
                r1 <- go fn
                r2 <- go xn
                pure $
                    case (r1, r2) of
                        (Left d1, Left d2) -> Left $ andDoc d1 d2
                        (Left d1, Right _) -> Left d1
                        (Right _, Left d2) -> Left d2
                        (Right f, Right x) -> Right $ f x

            go CEmpty = pure $ Left failVal
            go (CAlt x1 x2) = do
                r1 <- go x1
                case r1 of
                    Right x -> pure $ Right x
                    Left d1 -> do
                        r2 <- go x2
                        case r2 of
                            Right x -> pure $ Right x
                            Left d2 -> pure . Left $ orDoc d1 d2

            go (CSel x1 x2) = do
                r1 <- go x1
                case r1 of
                    Left d1 -> do
                        r2 <- go x2
                        pure $
                            case r2 of
                                Left d2 -> Left $ selDoc d1 d2
                                Right _ -> Left d1
                    Right (Right b) -> pure $ Right b
                    Right (Left a)  -> do
                        r2 <- go x2
                        pure $
                            case r2 of
                                Left d2 -> Left d2
                                Right f -> Right $ f a
                

            lmap :: forall x y z . (x -> z) -> Either x y -> Either z y
            lmap f (Left x)  = Left (f x)
            lmap _ (Right y) = Right y

            readvar :: forall x . ParseInfo x -> IO (ReadResult x)
            readvar pinfo = do
                r <- reader (name (envVar pinfo))
                pure $
                    case r of
                        NotFound -> NotFound
                        ReadError err -> ReadError err
                        Contents cnts ->
                            case parseFn pinfo cnts of
                                Left err -> ReadError err
                                Right x  -> Contents x


    -- | Generate a configuration by reading environment variables.
    --
    -- This is the original implementation.
    --
    -- The Html generated on error still needs to be passed to
    -- `generateWebPage`.  This is done so that the caller and add
    -- additional content before or after the main tree.
    runConfig :: Config a -> IO (Either Html a)
    runConfig = runConfigBase readEnv
        where
            readEnv :: VarName -> IO (ReadResult String)
            readEnv vname = fixup <$> lookupEnv (getVarName vname)

            fixup :: Maybe String -> ReadResult String
            fixup Nothing  = NotFound
            fixup (Just a) = Contents a

    -- | Generate a configuration value by reading files.
    --
    -- Given a directory to read from, and a charset decoder,
    -- generate the configuration.  Every variable is assumed to
    -- be a file of the same name in the given directory.
    --
    -- This is basically D.J. Berstein's envdir program inlined.
    --
    -- The Html generated on error still needs to be passed to
    -- `generateWebPage`.  This is done so that the caller and add
    -- additional content before or after the main tree.
    --
    runConfigDirBase :: (Lazy.ByteString -> Either String String)
                            -- ^ Charset decoder
                        -> FilePath
                            -- ^ Directory to read from
                        -> Config a
                            -- ^ Program to execute
                        -> IO (Either Html a)
    runConfigDirBase decode filePath = runConfigBase rFile
        where
            rFile :: VarName -> IO (ReadResult String)
            rFile vname = do
                let fpath :: FilePath
                    fpath = filePath </> getVarName vname
                b <- Dir.doesFileExist fpath
                if (not b)
                then pure NotFound
                else fixup <$> Lazy.readFile fpath

            fixup :: Lazy.ByteString -> ReadResult String
            fixup bs =
                case decode bs of
                    Left err -> ReadError err
                    Right s  -> Contents s

    -- | Generate a configuration value by reading files.
    --
    -- Given a directory to read from, generate the configuration. 
    -- Every variable is assumed to be a file of the same name in
    -- the given directory.  A default UTF-8 charset decoder is used.
    --
    -- This is basically D.J. Berstein's envdir program inlined.
    --
    -- The Html generated on error still needs to be passed to
    -- `generateWebPage`.  This is done so that the caller and add
    -- additional content before or after the main tree.
    --
    runConfigDir :: FilePath -> Config a -> IO (Either Html a)
    runConfigDir = runConfigDirBase decode
        where
            decode :: Lazy.ByteString -> Either String String
            decode bs =
                case Encoding.decodeUtf8' bs of
                    Right txt -> Right $ LazyText.unpack txt
                    Left  err -> Left $ "UTF-8 decoding error: " ++ show err


    -- | Generate documentation for a Config program.
    --
    -- The Html generated still needs to be passed to `generateWebPage`. 
    -- This is done so that the caller and add additional content before
    -- or after the main tree.
    helpConfig :: Config a -> Html
    helpConfig = generateDoc . loop
        where
            loop :: Config a -> Doc
            loop (CLabel summ desc cfg) = labelDoc summ desc (loop cfg)
            loop (CParse pinfo) = parseVar True pinfo []
            loop (CParseOpt pinfo) = parseVar False pinfo []
            loop (CParseDef a pinfo) =
                parseVar False pinfo [ ("Default:", fromString (show a)) ]
            loop (CMap _ cfg) = loop cfg
            loop (CPure _)    = pureVal
            loop (CApp c1 c2) = andDoc (loop c1) (loop c2)
            loop CEmpty       = failVal
            loop (CAlt c1 c2) = orDoc (loop c1) (loop c2)
            loop (CSel c1 c2) = selDoc (loop c1) (loop c2)

