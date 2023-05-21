
-- |
-- Module      : Config.Env
-- Description : Environment Variable based configuration
-- Copyright   : (c) Brian Hurt, 2023
-- License     : BSD 3-Clause
-- Maintainer  : Brian Hurt <bhurt42@gmail.com>
-- Stability   : experimental
-- 
-- Sort of like optparse-applicative, but for environment variables.
-- 
-- From experience, it's a lot easier to set up a terraform server that
-- gets it's config from environment variables- we just install D.J.
-- Bernstein's daemontools package, and then use the envdir command.
-- Configuration is then creating files in a specific directory with
-- the name of the file being the environment variable we want to create,
-- and the contents the value.  This is very easy to do in terraform.
-- Setting up command line arguments is more tricky.  Also, other tools
-- from that package are usefull as well.
-- 
-- But moving to environment variables over command line arguments means
-- that we lose the pleasure of working with optparse-applicative.  The
-- optparse-applicative package gives us a nice data type implementing
-- Applicative and Alternative that we can build a very complicated
-- command line parser from.
-- 
-- This module is the solution.  It creates a new type `Config`, which
-- implements applicative and alternative (and selective, a new class
-- which lies between applicative and monad in power).  This allows
-- us to build up a complicated parsing function.  We can then
-- autogenerate a help function OR execute the function and create
-- the configuration.
--
-- A little bit of generality actually grants us a huge increase in
-- power.  We can replace the @lookupEnv@ function with any function
-- that has the right type.  For example, we could just directly read
-- files on the filesystem, eliminating the need for envdir altogether.  
-- Or we could query some table in a database.
--
module Config.Env (

    -- * Main Types
    --
    -- | A @Config a@ represents a program that creates a configuration
    -- value of type @a@.
    --
    -- A Config program can read variables using one of the parsing
    -- functions, for example `readVar` or `stringVar`, etc.
    --
    -- Config implements Applicative, so you can use the standard
    -- pattern to construct records, like:
    --
    -- @
    --  data Foo = Foo {
    --                  foo :: String,
    --                  bar :: Int }
    --
    --  parseFoo :: Config Foo
    --  parseFoo = Foo
    --              <$> stringVar "MyFoo"
    --              <*> readVar "MyBar"
    -- @
    --
    -- Config implements Alternative, so variant types can be parsed
    -- like:
    --
    -- @
    --  data Quux = Yakko Foo
    --              | Wakko Int
    --              | Dot
    --
    --  parseQuux :: Config Quux
    --  parseQuux =
    --      (Yakko <$> parseFoo)
    --      <|> (Wakko <$> readVar "HelloNurse")
    --      <|> pure Dot
    -- @
    --
    -- Config does not implement Monad, as we need to be able to traverse
    -- the program and find all the variables that might be read to
    -- generate the help text.  As a compromise, we implement the
    -- Selective type class, which lets us choose how to create the
    -- configuation value.  For example, we might write:
    --
    -- @
    --  import Control.Selective (ifS)
    --
    --  parseWhatever :: Config Whatever
    --  parseWhatever = ifS (existsVar "WhichWhatever") onTrue onFalse
    --      where
    --          -- Executed when the WhichWhatever variable is set.
    --          onTrue :: Config Whatever
    --          onTrue = ...
    --
    --          -- Execute when the WhichWhatever variable is not set.
    --          onFalse = ...
    -- @
    --
    Config,
    VarName,
    Variable(..),

    -- * Reading Variables
    --
    -- | One might wonder why we don't define some type class like:
    --
    -- @
    --  class Parserable a where
    --      parse :: Variable -> Config a
    --
    --  instance Read a => Parserable a where
    --      parse = ...
    -- @
    --
    -- The problem with this is strings.  The read implementation for
    -- strings (including texts and bytestrings) requires the string
    -- be quoted and escaped.  So you can't just do:
    --
    -- @
    --      FOO="whatever"
    -- @
    --
    -- you have to do:
    --
    -- @
    --      FOO="\"whatever\""
    -- @
    --
    -- Which is annoying, to say the least.  We want to use the fromString
    -- implementation of the IsString typeclass to convert strings.  But
    -- if we then do:
    --
    -- @
    --      instance IsString a => Parseable a where
    --          parse = ...
    -- @
    --
    -- This leads us into overlapping isntances hell.  And right quick
    -- too, as String implements both Read and IsString.  We can't
    -- ditch the Read interface, because we still need it for types
    -- like Int.  And once we add the FromJSON option to parse
    -- environment variables, life becomes even more difficult.
    --
    -- The solution is to just have different functions that explicitly
    -- encode which typeclass they use to decode the variable.
    --
    -- ** Required Variables
    --
    -- | These functions fail if the variable does not exist.
    --
    readVar,
    stringVar,
    jsonVar,

    -- ** Optional Variables
    --
    -- | These functions return Nothing if the variable does not exist.
    --
    -- Note that they can still fail, if the variable can not be parsed.
    --
    readVarOpt,
    stringVarOpt,
    jsonVarOpt,

    -- ** Optional Variables With Defaults
    --
    -- | These functions return a default value if the variable does not
    -- exist.
    --
    -- The type being parsed needs to implement the Show typeclass, so
    -- documentation can print the default value.
    --
    -- Note that they can still fail, if the variable can not be parsed.
    --
    readVarDef,
    stringVarDef,
    jsonVarDef,

    -- ** Testing Variable Existence
    varExists,

    -- * Labelling Configs
    label,

    -- * Running Configs
    runConfig,
    runConfigDir,
    helpConfig,

    -- * Low-level Functions
    --
    -- | These are functions which are exported to allow extending this
    -- library.  In normal use, you probably do not need them.
    
    -- ** Low-level Parsers
    parseVar,
    parseVarOpt,
    parseVarDef,

    -- ** Low-level Runners
    runConfigDirBase,
    runConfigBase,
    ReadResult(..)

) where

    import           Config.Env.Internal.Config
    import           Config.Env.Internal.Parsers
    import           Config.Env.Internal.Run

