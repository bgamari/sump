{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding (mapM_, concatMap)
import Data.Foldable
import Data.Bits
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Lens hiding (Level, (#), none)
import Numeric.Lens (hex)

import qualified Data.Vector as V
import System.Hardware.Sump as Sump

import Data.Default
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Graphics.SVGFonts.ReadFont

import I2c
import qualified Data.Machine as M

printing :: Show a => IO a -> IO ()
printing action = action >>= print

acquire :: FilePath -> EitherT String IO (V.Vector Sample)
acquire path = do
    sump <- Sump.open path
    Sump.identify sump >>= liftIO . print
    Sump.setDivider sump 0x80
    Sump.setFlags sump def
    let trig = Sump.levelTrigger [(ch 0, Low)]
    Sump.configureTrigger sump Stage0 trig
    Sump.setReadDelayCounts sump 0x0110 0x0100
    Sump.run sump

main :: IO ()
main = printing $ runEitherT $ do
    --samples <- acquire "/dev/ttyACM0"
    --liftIO $ writeFile "samples.out" $ show samples
    samples <- liftIO (fmap read $ readFile "samples.out" :: IO (V.Vector Sample))

    let i2cSignals = map (\s->I2cSignals (channelLevel (ch 0) s)
                                         (channelLevel (ch 1) s))
                     $ V.toList samples
        events = M.run $ M.supply i2cSignals (M.auto decode)
    --liftIO $ print $ toListOf (each . _1 . _Right . transferWord . re hex) transfers

    --let channels = [minBound..maxBound]
    let channels = [ ch 0, ch 1 ]
        i2c = drawI2c events
    liftIO $ renderSVG "out.svg"
                       (mkSizeSpec Nothing Nothing)
                       (vcat' (def & sep .~ 1)
                        [i2c, scaleY 10 $ samplesToDiagram channels samples])
    return () :: EitherT String IO ()

drawI2c :: (Renderable (Path R2) b, Backend b R2)
        => [Maybe I2cEvent] -> Diagram b R2
drawI2c events = 
    let eventPic ev = circle 2 # lw none # fc color
          where
            color = case ev of
                      I2c.Start   -> yellow
                      Stop        -> blue
                      (Bit _)     -> black
                      (Invalid _) -> red
        eventDia = mconcat $ zipWith (\t v->maybe mempty eventPic v # translateX t) [0..] events
        transfers = decodeTransfers [(n, t) | (n, Just t) <- zip [Time 0..] events]
        wordDia (Right (Transfer word status)) length =
            stroke (textSVG (word ^. re hex) 14)
            # fc black # lw none # translateY 4
            ===
            rect (realToFrac length) 1 # lw none # fc blue # opacity 0.6
        wordDia _ _ = mempty
        transfersDia =
            mconcat
            $ map (\(transfer, Time start, Time end) ->
                      wordDia transfer (end - start)
                      # translateX (realToFrac start + 32))
            $ transfers
    in vcat [transfersDia # padY 2, eventDia]

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
