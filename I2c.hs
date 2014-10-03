{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module I2c
    ( I2cSignals (..)
    , I2cEvent (..)
    , _Start, _Bit, _Stop, _Invalid
    , decode
    , count
      -- * Transaction parsing
    , AckNack (..)
    , _Ack, _Nack
    , Transfer (..)
    , transferWord, transferStatus
    , decodeTransfers
    ) where

import Debug.Trace
import Prelude hiding ((.))
import Control.Category
import Control.Applicative
import Data.Bits
import Data.Foldable
import Data.Traversable
import Data.Word

import Control.Lens hiding (Level)
import Data.Machine hiding (Stop)

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

count :: Mealy a b -> Mealy a (Int, b)
count = go 0
  where
    go n x = Mealy $ \a->let (xr, x') = runMealy x a
                         in ((n, xr), go (n+1) x')

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

data Transfer = Transfer { _transferWord :: Word8
                         , _transferStatus :: AckNack
                         }
              deriving (Show)
makeLenses ''Transfer

-- | Extract the transferred bytes from a stream of I2C events
decodeTransfers :: [(Time, I2cEvent)] -> [(Either String Transfer, Time, Time)]
decodeTransfers = findNext
  where
    -- drop until event after first start
    findNext :: [(Time, I2cEvent)] -> [(Either String Transfer, Time, Time)]
    findNext []  = []
    findNext evs = beginTransfer $ drop 1 $ dropWhile (isn't _Start . snd) evs

    beginTransfer :: [(Time, I2cEvent)] -> [(Either String Transfer, Time, Time)]
    beginTransfer evs@((t,_) : _) = readTransfer t 8 0 evs
    beginTransfer []              = []

    readTransfer :: Time -> Int -> Word8
                 -> [(Time, I2cEvent)] -> [(Either String Transfer, Time, Time)]
    readTransfer startT _    _    []                =
        (Left "Incomplete transfer", startT, startT) : []
    readTransfer startT 0 word ((n, Bit b):rest)    =
        (Right $ Transfer word status, startT, n) : endTransfer rest
      where
        status = case b of
                     Low  -> Ack
                     High -> Nack
    readTransfer startT n    word ((_, Bit b):rest) =
        readTransfer startT (n-1) (word .|. (toNum b `shiftL` (n-1))) rest
      where
        toNum Low  = 0
        toNum High = 1
    readTransfer _ _    word ((n,ev):rest) =
        (Left ("Invalid event during transfer: "++show ev), n, n) : findNext rest

    endTransfer :: [(Time, I2cEvent)] -> [(Either String Transfer, Time, Time)]
    endTransfer ((_, Stop):rest) = findNext rest
    endTransfer events           = beginTransfer events

-- | Construct an integer from its bits, least significant bit first
decodeInt :: (Num a, Bits a) => [Level] -> a
decodeInt = foldl' (.|.) 0 . zipWith bitOf [0..]
  where
    bitOf i High = bit i
    bitOf _ Low  = 0
