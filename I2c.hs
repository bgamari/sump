{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module I2c
    ( I2cSignals (..)
    , I2cEvent (..)
    , _Start, _Bit, _Stop, _Invalid
    , decode
      -- * Transaction parsing
    , AckNack (..)
    , _Ack, _Nack
    , Transaction (..)
    , transactions
    ) where

import Prelude hiding ((.))
import Control.Category
import Control.Applicative
import Data.Bits
import Data.Foldable
import Data.Traversable
import Data.Word

import Control.Lens hiding (Level)
import Data.Machine hiding (Stop)
import Data.Profunctor

import System.Hardware.Sump

data I2cSignals a = I2cSignals { scl, sda :: a }
                  deriving (Show, Functor, Foldable, Traversable)

instance Applicative I2cSignals where
    pure x = I2cSignals x x
    I2cSignals a b <*> I2cSignals x y = I2cSignals (a x) (b y)

data I2cEvent = Start
              | Bit Level
              | Stop
              | Invalid String
              deriving (Show)

makePrisms ''I2cEvent

data Edge = Falling | Rising
          deriving (Show, Eq, Ord)

startLevel :: Edge -> Level
startLevel Falling = High
startLevel Rising  = Low

finalLevel :: Edge -> Level
finalLevel = invert . startLevel

invert :: Level -> Level
invert Low = High
invert High = Low

edges :: Mealy Level (Maybe Edge)
edges = Mealy $ \l -> (Nothing, go l)
  where
    go l0 = Mealy $ \l1 -> case (l0, l1) of
                             (High, Low ) -> (Just Falling, go Low )
                             (Low,  High) -> (Just Rising,  go High)
                             (_,    l   ) -> (Nothing,      go l   )

mapMealy :: (Applicative f) => Mealy a b -> Mealy (f a) (f b)
mapMealy f = go (pure f)
  where
    go p = Mealy $ \x->let r = runMealy <$> p <*> x
                       in (fmap fst r, go $ fmap snd r)


zipMealy :: Mealy a b -> Mealy a c -> Mealy a (b,c)
zipMealy x y = go x y
  where
    go x y = Mealy $ \a -> let (xr, x') = runMealy x a
                               (yr, y') = runMealy y a
                           in ((xr, yr), go x' y')

cat :: Mealy a a
cat = go where go = Mealy $ \s -> (s, go)

decode :: Mealy (I2cSignals Level) (Maybe I2cEvent)
decode = idle . mapMealy (zipMealy cat edges)
  where
    idle, waitForClkLowOrStop, latchBit
      :: Mealy (I2cSignals (Level, Maybe Edge)) (Maybe I2cEvent)
    idle = Mealy $ \s->
        case s of
          I2cSignals (_,Nothing) (_,Just Falling) -> (Just Start, waitForClkLowOrStop)
          I2cSignals _ (_,Just Falling)           -> ( Just $ Invalid "Simultaneous edges"
                                                     , idle)
          _                                       -> (Nothing, idle)

    waitForClkLowOrStop = Mealy $ \s->
        case s of
          I2cSignals (_,Just Falling) _       -> (Nothing, latchBit)
          I2cSignals (High,_) (_,Just Rising) -> (Just Stop, idle)
          _                                   -> (Nothing, waitForClkLowOrStop)

    latchBit = Mealy $ \s->
        case s of
          I2cSignals (_,Just Rising) (bit,_)  -> (Just $ Bit bit, waitForClkLowOrStop)
          _                                   -> (Nothing, latchBit)


data AckNack = Ack | Nack
             deriving (Show, Ord, Eq, Bounded)

makePrisms ''AckNack

data Transaction = Trans { transWord :: Word8
                         , transStatus :: AckNack
                         }
                 deriving (Show)

transactions :: [I2cEvent] -> [(Transaction, Int, Int)]
transactions = go . zip [0..]
  where
    go :: [(Int, I2cEvent)] -> [(Transaction, Int, Int)]
    go events
      | [] <- events' = []
      | [] <- bits    = go rest
      | otherwise     =
        (trans, fst $ head events', fst $ last bits) : go rest
      where
        events' = dropWhile (isn't _Start . snd) events
        (bits, rest) = break (not . isn't _Stop . snd) events'
        status = case snd $ last bits of
                     Bit Low  -> Ack
                     Bit High -> Nack
        word = decodeInt $ toListOf (each . _2 . _Bit) $ init bits
        trans = Trans { transWord = word
                      , transStatus = status
                      }

-- | Construct an integer from its bits, least significant bit first
decodeInt :: (Num a, Bits a) => [Level] -> a
decodeInt = foldl' (.|.) 0 . zipWith bitOf [0..]
  where
    bitOf i High = bit i
    bitOf _ Low  = 0
