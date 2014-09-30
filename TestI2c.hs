import System.Hardware.Sump as Sump
import I2c
import Data.Machine

parseLevel '0' = Low
parseLevel '1' = High

signals = 
    let sda = map parseLevel "1110001110001111"
        scl = map parseLevel "1111100100111111"
    in zipWith I2cSignals scl sda

main = mapM_ print $ Data.Machine.run $ supply signals (auto decode)
