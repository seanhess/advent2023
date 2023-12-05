module App.Prelude
  ( module Prelude
  , (&)
  , (<|>)
  , cs
  , module Control.Monad.IO.Class
  , module Data.Maybe
  , module Data.String
  , Generic
  , Map
  , Text
  , UTCTime

    -- * List functions
  , module Data.List
  , module Data.Ord
  , module Data.Proxy

    -- * Monadic functions
  , module Control.Monad

    -- * Effects
  , Eff
  , (:>)

    -- * Lifted IO
  , putStrLn
  , print
  , putStr
  , readFile
  , Identity
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, guard, unless, when, zipWithM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import Data.Functor.Identity (Identity)
import Data.List (find, group, groupBy, sortOn)
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Proxy (Proxy (..))
import Data.String (IsString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Effectful
import GHC.Generics (Generic)
import Prelude hiding (Real, print, putStr, putStrLn, readFile, reverse, writeFile)
import Prelude qualified

print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . Prelude.print

putStrLn :: (MonadIO m) => String -> m ()
putStrLn = liftIO . Prelude.putStrLn

putStr :: (MonadIO m) => String -> m ()
putStr = liftIO . Prelude.putStr

readFile :: (MonadIO m) => FilePath -> m String
readFile = liftIO . Prelude.readFile
