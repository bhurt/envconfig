
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
    Config,
    VarName,
    Variable(..),
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
    import           Config.Env.Internal.Parsers

