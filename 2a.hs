{-# LANGUAGE OverloadedStrings #-}
import Data.Functor
import qualified Data.Text.IO as TIO
import Protein

main :: IO ()
main = TIO.putStrLn =<< head . translate <$> TIO.getLine

