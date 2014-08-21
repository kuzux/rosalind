import Data.Functor
import Newick
import qualified Data.Text.IO as TIO

geno :: Tree -> (Double, Double, Double)
geno (Leaf (Just "AA")) = (1,0,0)
geno (Leaf (Just "Aa")) = (0,1,0)
geno (Leaf (Just "aa")) = (0,0,1)
geno (Internal _ (x:y:_)) = (homoA, hetero, homoa)
    where (homoAX,heteroX,homoaX) = geno x
          (homoAY,heteroY,homoaY) = geno y
          homoA = homoAX*homoAY + 0.5*(heteroX*homoAY+homoAX*heteroY) + 0.25*heteroX*heteroY
          hetero = 0.5*(heteroX*homoAY+homoAX*heteroY+heteroX*heteroY+heteroX*homoaY+homoaX*heteroY) + homoAX*homoaY + homoaX*homoAY
          homoa = homoaX*homoaY + 0.5*(heteroX*homoaY+homoaX*heteroY) + 0.25*heteroX*heteroY

main :: IO ()
main =  putStrLn =<< disp . either error geno . parseTree <$> TIO.getContents
    where disp (x,y,z) = unwords . map show $ [x,y,z]
