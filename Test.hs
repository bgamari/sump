import Data.Default
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import System.Hardware.Sump as Sump

printing :: Show a => IO a -> IO ()
printing action = action >>= print

main :: IO ()
main = printing $ runEitherT $ do
    sump <- Sump.open "/dev/ttyACM1"
    Sump.identify sump >>= liftIO . print
    --Sump.setDivider sump 4
    Sump.setFlags sump def
    let trig = Sump.serialTrigger [(ch 0, Low)]
    Sump.configureTrigger sump Stage0 trig
    Sump.setReadDelayCounts sump 0xffff 0
    Sump.run sump >>= liftIO . print
    return ()
