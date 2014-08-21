{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor
import Data.Vector (Vector, (!))
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Text as T
import Fasta

data KmpState = KmpState { pos :: Int , cnd :: Int , res :: Vector Int }
type Kmp a = ReaderT Text (StateT KmpState Identity) a

execKmp :: Kmp a -> Text -> KmpState
execKmp m xs = runIdentity . (flip execStateT $ initState) . (flip runReaderT $ xs) $ m
    where initState = KmpState 2 0 (V.fromList [-1, 0])

kmp :: (MonadState KmpState m, MonadReader Text m) => m ()
kmp = do
    text <- ask
    KmpState p c r <- get
    case () of _
                 | p > T.length text                        -> do
                       return ()
                 | text `T.index` (p-1) == text `T.index` c -> do
                       put $ KmpState (p+1) (c+1) (r `V.snoc` (c+1))
                       kmp
                 | c > 0                                    -> do
                       put $ KmpState p (r ! c) r
                       kmp
                 | otherwise                                -> do
                      put $ KmpState (p+1) 0 (r `V.snoc` 0)
                      kmp

main :: IO ()
main = do
    shite <- snd . head . parseFastas <$> getContents
    putStrLn . unwords . map show . V.toList . V.tail . res . execKmp kmp $ shite
