{-# LANGUAGE FlexibleContexts #-}

import Data.Bits
import Data.List (foldl')
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Lens hiding (Level, (#))

import qualified Data.Vector as V
import System.Hardware.Sump as Sump

import Data.Default
import Diagrams.Prelude
import Diagrams.Backend.SVG

printing :: Show a => IO a -> IO ()
printing action = action >>= print

main :: IO ()
main = printing $ runEitherT $ do
    sump <- Sump.open "/dev/ttyACM0"
    liftIO $ print (def :: Flags)
    Sump.identify sump >>= liftIO . print
    Sump.setDivider sump 0x20
    Sump.setFlags sump def
    let trig = Sump.levelTrigger [(ch 0, Low)]
    Sump.configureTrigger sump Stage0 trig
    Sump.setReadDelayCounts sump 0x0110 0x0100
    samples <- Sump.run sump
    
    --let channels = [minBound..maxBound]
    let channels = [ ch 0, ch 1 ]
    liftIO $ renderSVG "out.svg"
                       (mkSizeSpec Nothing Nothing)
                       (scaleY 10 $ samplesToDiagram channels samples)
    return ()

samplesToDiagram :: Renderable (Path R2) b
                 => [Channel] -> V.Vector Sample -> Diagram b R2
samplesToDiagram channels samples =
    vcat' (def & sep .~ 1)
    $ map (\ch->strokeTrail (traceLevels $ map (channelLevel ch) $ V.toList samples)) channels
    ++ [timeBar]
  where
    timeBar = mconcat [ fromVertices [ t^&0, t^&0.5     ]
                      | t <- [0..realToFrac $ V.length samples] ] # lw thin

traceLevels :: [Level] -> Trail R2
traceLevels levels =
    trailFromVertices
    $ concatMap (\(t,l) -> [p2 (t, l), p2 (t+1, l)])
    $ zip [0..] $ map levelToCoord levels
  where
    levelToCoord Low  = 0
    levelToCoord High = 1
