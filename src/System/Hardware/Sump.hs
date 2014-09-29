{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Hardware.Sump
    ( -- * Basic Types
      Level (..)
    , Channel
    , ch
    , ChannelGroup (..)
    , Stage (..)
      -- * Initialization
    , Sump
    , open
    , reset
    , ProtocolVersion (..)
    , identify
      -- * Trigger configuration
    , Trigger (..)
    , levelTrigger
    , configureTrigger
      -- * Other configuration
    , setDivider
    , setReadDelayCounts
    , Flags (..)
    , setFlags
      -- * Acquisition
    , Sample (..)
    , channelLevel
    , run
    ) where

import Data.Char (ord, chr)
import Data.Bits
import Data.Word
import Data.List (foldl')
import Control.Monad (replicateM, void)
import Numeric (showHex)

import Control.Monad.Trans.Either
import Control.Monad.IO.Class

import qualified Data.Vector as V
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import Data.Default

import System.Hardware.Serialport

data ProtocolVersion = Version0
                     | Version1
                     | VersionUnknown Char
                     deriving (Eq, Ord, Show)

-- | A connection to an SUMP-compatible logic analyzer
data Sump = Sump
    { sumpDevice :: SerialPort
    }


open :: FilePath -> EitherT String IO Sump
open path = do
    s <- liftIO $ openSerial path settings
    liftIO $ flush s
    let sump = Sump s
    replicateM 5 $ command [0x0] 0 sump -- reset
    return sump
  where
    settings = defaultSerialSettings { commSpeed = CS115200
                                     , timeout = 1
                                     }

command :: [Word8] -> Int -> Sump -> EitherT String IO ByteString
command c replyLen (Sump sump) = do
    liftIO $ putStrLn $ concat $ map (flip showHex " ") c
    liftIO $ send sump (BS.pack c)
    reply <- liftIO $ recv sump replyLen
    return reply

reset :: Sump -> EitherT String IO ()
reset = void . command [0x0] 0

newtype Sample = Sample Word32

instance Show Sample where
    show (Sample s) = showHex s ""

channelLevel :: Channel -> Sample -> Level
channelLevel (Ch c) (Sample s)
  | bit c .&. s == 0  = Low
  | otherwise         = High

readSample :: Sump -> EitherT String IO (Maybe Sample)
readSample sump = do
    s <- liftIO $ recv (sumpDevice sump) 4
    liftIO $ print s
    case BS.unpack s of
      bs@[_,_,_,_] -> right $ Just $ Sample
                        $ foldl' (\accum a->(accum `shiftL` 8) .|. fromIntegral a) 0 bs
      []           -> right Nothing
      _            -> left "Unknown response"

run :: Sump -> EitherT String IO (V.Vector Sample)
run sump = do
    command [0x1] 0 sump
    let go accum = do
          ss <- readSample sump
          case ss of
            Just s -> go $ V.snoc accum s
            Nothing -> return accum

    let readFirst = do
          ss <- readSample sump
          case ss of
            Just s -> return s
            Nothing -> readFirst

    first <- readFirst
    go (V.singleton first)

identify :: Sump -> EitherT String IO (ByteString, ProtocolVersion)
identify sump = do
    reply <- BS.reverse `fmap` command [0x2] 4 sump
    let device = BS.take 3 reply
    version <- case map (chr . fromIntegral) $ BS.unpack $ BS.drop 3 reply of
                   ['0'] -> right Version0
                   ['1'] -> right Version1
                   [c]   -> right $ VersionUnknown c
                   []    -> left "No reply to ID command"
    return (device, version)

byte :: (Integral a, Bits a) => Int -> a -> Word8
byte b a = fromIntegral $ a `shiftR` (8*b)

word32Bytes :: Word32 -> [Word8]
word32Bytes v = [byte 0 v, byte 1 v, byte 2 v, byte 3 v]

setDivider :: Sump -> Int -> EitherT String IO ()
setDivider sump d =
    void $ command [0x80, byte 0 d, byte 1 d, byte 2 d, 0] 0 sump

-- | A trigger stage
data Stage = Stage0 | Stage1 | Stage2 | Stage3
           deriving (Eq, Ord, Bounded, Enum, Show)

-- | A logical high or low
data Level = High | Low
              deriving (Eq, Ord, Bounded, Enum, Show)

-- | A logic analyzer channel
newtype Channel = Ch Int
              deriving (Eq, Ord, Enum, Show)

instance Bounded Channel where
    minBound = Ch 0
    maxBound = Ch 32

-- | Construct a channel
ch :: Int -> Channel
ch c
  | c >= minBound && c < maxBound = Ch c
  | otherwise                     = error "Invalid channel"

channelBit :: Bits a => Channel -> a
channelBit (Ch c) = bit c

data Trigger
    = SerialTrigger { triggerDelay    :: Word16
                    , triggerLevel    :: Word8
                    , triggerChannel  :: Channel
                    , triggerStart    :: Bool
                    , triggerMask     :: Word32
                    , triggerValue    :: Word32
                    }
    | ParallelTrigger { triggerDelay   :: Word16
                      , triggerLevel   :: Word8
                      , triggerStart   :: Bool
                      , triggerValues  :: [(Channel, Level)]
                      }

-- | Trigger on the simultaneous levels of a set of channels
levelTrigger :: [(Channel, Level)] -> Trigger
levelTrigger values =
    ParallelTrigger { triggerDelay  = 0
                    , triggerLevel  = 0
                    , triggerStart  = True
                    , triggerValues = values
                    }

configureTrigger :: Sump
               -> Stage
               -> Trigger
               -> EitherT String IO ()
configureTrigger sump stage config@(SerialTrigger {}) = do
    command (forStage 0xc0 : word32Bytes (triggerMask config)) 0 sump
    command (forStage 0xc1 : word32Bytes (triggerValue config)) 0 sump
    command [ forStage 0xc2
            , byte 0 (triggerDelay config)
            , byte 1 (triggerDelay config)
            , fromIntegral (0xf .&. fromEnum (triggerChannel config))
              .|. fromIntegral (fromEnum $ triggerLevel config)
            ,     if triggerStart config then 0x8 else 0
              .|. 0x4
              .|. fromIntegral (0xf .&. (fromEnum $ triggerChannel config) `shiftR` 4)
            ] 0 sump
    return ()
  where
    forStage :: Word8 -> Word8
    forStage cmd = cmd .|. (fromIntegral (fromEnum stage) `shiftR` 2)

configureTrigger sump stage config@(ParallelTrigger {triggerValues=trigger}) = do
    let mask = foldl' (.|.) 0 $ map (channelBit . fst) trigger
        values = foldl' (.|.) 0
               $ map (\(c,v)->case v of
                                High -> bit $ channelBit c
                                Low  -> 0)
               $ trigger
    command (forStage 0xc0 : word32Bytes mask) 0 sump
    command (forStage 0xc1 : word32Bytes values) 0 sump
    command [ forStage 0xc2
            , byte 0 (triggerDelay config)
            , byte 1 (triggerDelay config)
            , fromIntegral (fromEnum $ triggerLevel config)
            , if triggerStart config then 0x8 else 0
            ] 0 sump
    return ()
  where
    forStage :: Word8 -> Word8
    forStage cmd = cmd .|. (fromIntegral (fromEnum stage) `shiftR` 2)

setReadDelayCounts :: Sump
                   -> Word16 -- ^ Read count
                   -> Word16 -- ^ Delay count
                   -> EitherT String IO ()
setReadDelayCounts sump read delay = do
    let c = [0x81, byte 0 read, byte 1 read, byte 0 delay, byte 1 delay]
    void $ command c 0 sump

data ChannelGroup = ChGrp0 | ChGrp1 | ChGrp2 | ChGrp3
                  deriving (Show, Eq, Ord, Bounded, Enum)

data Flags = Flags { demux :: Bool
                   , inputFilter :: Bool
                   , enabledGroups :: [ChannelGroup]
                   , externalClock :: Bool
                   , invertedClock :: Bool
                   }
           deriving (Show)

setFlags :: Sump -> Flags -> EitherT String IO ()
setFlags sump flags = do
    let groups = enabledGroups flags
        v = foldl' (.|.) 0
            [ bit 0 `is` demux flags
            , bit 1 `is` inputFilter flags
            , bit 2 `is` (ChGrp0 `notElem` groups)
            , bit 3 `is` (ChGrp1 `notElem` groups)
            , bit 4 `is` (ChGrp2 `notElem` groups)
            , bit 5 `is` (ChGrp3 `notElem` groups)
            , bit 6 `is` externalClock flags
            , bit 7 `is` invertedClock flags
            ]
        b `is` True  = bit b
        b `is` False = 0
    command [0x82, v, 0, 0, 0] 0 sump
    return ()

-- | All groups enabled, internal clock, no demux or input filter
instance Default Flags where
    def = Flags { demux = False
                , inputFilter = False
                , enabledGroups = [minBound .. maxBound]
                , externalClock = False
                , invertedClock = False
                }
