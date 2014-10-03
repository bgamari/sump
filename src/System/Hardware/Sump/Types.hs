{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module System.Hardware.Sump.Types
    ( -- * Logical levels
      Level (..)
    , _Low, _High
    , invert
      -- * Logical transitions
    , Edge (..)
    , startLevel
    , finalLevel
      -- * Time
    , Time (..)
    ) where

import Control.Lens hiding (Level)

-- | A logical high or low
data Level = High | Low
              deriving (Eq, Ord, Bounded, Enum, Show)

makePrisms ''Level

invert :: Level -> Level
invert Low = High
invert High = Low

-- | A notion of discrete time
newtype Time = Time Int
             deriving (Show, Ord, Eq, Enum)
makeWrapped ''Time

data Edge = Falling | Rising
          deriving (Show, Eq, Ord)
     
makePrisms ''Edge

startLevel :: Edge -> Level
startLevel Falling = High
startLevel Rising  = Low

finalLevel :: Edge -> Level
finalLevel = invert . startLevel
