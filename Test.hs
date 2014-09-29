import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import System.Hardware.Sump as Sump

printing :: Show a => IO a -> IO ()
printing action = action >>= print

main :: IO ()
main = printing $ runEitherT $ do
    sump <- Sump.open "/dev/ttyACM1"
    Sump.identify sump >>= liftIO . print
    return ()
