{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}

module Spi
    ( -- * Types
      SyncSignals (..)
    , InOut (..)
      -- * Decoding
    , decode
    ) where

import Prelude hiding ((.), id)
import Control.Category
import Control.Applicative
import Control.Monad (forever)
import Data.Foldable
import Data.Traversable

import Control.Lens hiding (Level)
import Data.Machine

import System.Hardware.Sump.Types

-- | Synchronous logic signals and their clock
data SyncSignals f a = SyncSignals
    { sclk :: a
    , signals :: f a
    }
    deriving (Show, Functor, Foldable, Traversable)

instance Applicative f => Applicative (SyncSignals f) where
    pure x = SyncSignals x (pure x)
    SyncSignals a b <*> SyncSignals x y = SyncSignals (a x) (b <*> y)

-- | Standard SPI MOSI and MISO signals
data InOut a = InOut { mosi, miso :: a }
             deriving (Show, Functor, Foldable, Traversable)

instance Applicative InOut where
    pure x = InOut x x
    InOut a b <*> InOut x y = InOut (a x) (b y)

dropUntilEdge :: (a -> Level) -> Edge -> Plan (Is a) a ()
dropUntilEdge getLevel edge = await >>= go . getLevel
  where
    -- go :: Level -> Plan (Is a) a ()
    go l0 = do
      a <- await
      let l1 = getLevel a
      case (l0, l1) ^? edgeLevels of
        Just edge' | edge == edge' -> yield a
        _                          -> go l1

decode :: Edge -> Process (SyncSignals f Level) (f Level)
decode edge =
    wait ~> emitBits
  where
    wait = repeatedly $ do
               dropUntilEdge sclk edge
               await >>= yield
    emitBits = repeatedly $ await >>= yield . signals
