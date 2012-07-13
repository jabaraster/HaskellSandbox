module Oidashi where

import System.Random
import Data.List
import Data.Maybe
import qualified Data.Map as M

data MosaicEnv = MosaicEnv { tiles::(M.Map Position Tile), pixels::[Pixel] }
                 deriving (Show, Eq, Read)

data Image = Image { imageColor::Color, oid::Integer }
             deriving (Show, Eq, Read)

type Position = (Int, Int)

data Tile = Tile { pixel::Pixel, image::Image }
            deriving (Show, Eq, Read)

data Pixel = Pixel { position::Position, color::Color }
             deriving (Show, Eq, Read)

data Color = Color { red::Int, green::Int, blue::Int }
             deriving (Show, Eq, Read)

insertTile :: MosaicEnv -> Tile -> MosaicEnv
insertTile env t = env { tiles = M.insert (position $ pixel t) t (tiles env)}

lookupTile :: MosaicEnv -> Position -> Maybe Tile
lookupTile env p = M.lookup p (tiles env)

(/-):: Color -> Color -> Int
a /- b = abs (red a - red b) + abs (green a - green b) + abs (blue a - blue b)

testData = [(Pixel (x,y) (Color ((x+1)*10) ((y+1)*10) ((x+y+2)*10))) | x <- [0..3], y <- [0..3]]

testData' = do
  r <- getStdRandom $ randomR (0, 255)
  g <- getStdRandom $ randomR (0, 255)
  b <- getStdRandom $ randomR (0, 255)
  return $ [Pixel (x,y) (Color r g b) | x <- [0..3], y <- [0..3]]

sortPixels :: [Pixel] -> Color -> [Pixel]
sortPixels [] _ = []
sortPixels ps c = sortBy (\ l r -> compare (color l /- c) (color r /- c)) ps

lookupPixel :: [Pixel] -> Color -> Pixel
lookupPixel [] _ = error "empty list not supported."
lookupPixel ps c = (sortPixels ps c) !! 0

addColor :: MosaicEnv -> Image -> MosaicEnv
addColor env i = let p = lookupPixel (pixels env) (imageColor i)
  in case lookupTile env (position p) of
    Just pos -> env
    Nothing  -> insertTile env (Tile p i)

test_lookupPixel_01 r g b = elem (lookupPixel testData (Color r g b)) testData
test_lookupPixel_02 r g b = let c = Color r g b
                                min = lookupPixel testData c
                                sd = sortPixels testData c
                                mIdx = elemIndex min sd
                                ps = drop ((fromMaybe (-1) mIdx)+1) sd
                                dis = color min /- c
  in case ps of
    [] -> error "empty list not supported."
    otherwise -> all (\p -> (color p /- c) > dis) ps
  where
    remove :: [Pixel] -> Pixel -> [Pixel]
    remove ps p = filter (/=p) ps
