{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module System.Hardware.Sump.Types
    ( Level (..)
    , _Low, _High
    , Time (..)
    ) where

import Control.Lens hiding (Level)

-- | A logical high or low
data Level = High | Low
              deriving (Eq, Ord, Bounded, Enum, Show)

makePrisms ''Level

-- | A notion of discrete time
newtype Time = Time Int
             deriving (Show, Ord, Eq, Enum)
makeWrapped ''Time
