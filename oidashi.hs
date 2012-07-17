module Oidashi where

import System.Random
import Data.List
import Data.Maybe
import qualified Data.Map as M

data MosaicEnv = MosaicEnv { tiles::(M.Map Location Tile), pixels::[Pixel] }
                 deriving (Show, Eq, Read)

data Image = Image { imageColor::Color, oid::Integer }
             deriving (Show, Eq, Read)

type Location = (Int, Int)

data Tile = Tile { pixel::Pixel, image::Image }
            deriving (Show, Eq, Read)

data Pixel = Pixel { position::Location, color::Color }
             deriving (Show, Eq, Read)

data Color = Color { red::Int, green::Int, blue::Int }
             deriving (Show, Eq, Read)

insertTile :: MosaicEnv -> Tile -> MosaicEnv
insertTile env t = env { tiles = M.insert (position $ pixel t) t (tiles env)}

lookupTile :: MosaicEnv -> Location -> Maybe Tile
lookupTile env p = M.lookup p (tiles env)

(/-):: Color -> Color -> Int
l /- r = abs (red l - red r) + abs (green l - green r) + abs (blue l - blue r)

createTestData' :: [Location] -> IO [Pixel]
createTestData' ls = do
  cs <- f (length ls) []
  return $ zipWith Pixel ls cs
  where
    f :: Int -> [Color] -> IO [Color]
    f 0 ps = return ps
    f i ps = do
      r <- getStdRandom $ randomR (0, 255)
      g <- getStdRandom $ randomR (0, 255)
      b <- getStdRandom $ randomR (0, 255)
      c <- return $ Color r g b
      cs <- f (i-1) ps
      return (c:cs)

createTestData = do
  ls <- return [(x,y)|x<-[0..3],y<-[0..3]]
  return $ createTestData ls

testData = [
  Pixel {position = (0,0), color = Color {red = 136, green = 39, blue = 186}}
  ,Pixel {position = (0,1), color = Color {red = 112, green = 24, blue = 146}}
  ,Pixel {position = (0,2), color = Color {red = 76, green = 111, blue = 6}}
  ,Pixel {position = (0,3), color = Color {red = 136, green = 212, blue = 205}}
  ,Pixel {position = (1,0), color = Color {red = 59, green = 33, blue = 136}}
  ,Pixel {position = (1,1), color = Color {red = 19, green = 0, blue = 176}}
  ,Pixel {position = (1,2), color = Color {red = 45, green = 208, blue = 169}}
  ,Pixel {position = (1,3), color = Color {red = 25, green = 94, blue = 63}}
  ,Pixel {position = (2,0), color = Color {red = 187, green = 27, blue = 47}}
  ,Pixel {position = (2,1), color = Color {red = 172, green = 178, blue = 64}}
  ,Pixel {position = (2,2), color = Color {red = 151, green = 141, blue = 84}}
  ,Pixel {position = (2,3), color = Color {red = 132, green = 130, blue = 153}}
  ,Pixel {position = (3,0), color = Color {red = 252, green = 179, blue = 150}}
  ,Pixel {position = (3,1), color = Color {red = 176, green = 30, blue = 179}}
  ,Pixel {position = (3,2), color = Color {red = 204, green = 254, blue = 40}}
  ,Pixel {position = (3,3), color = Color {red = 66, green = 113, blue = 101}}
  ]

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
                                dis = color min /- c
                                min = lookupPixel testData c
                                -- 最近傍を除いたピクセルの配列を得る.
                                -- この配列内の全ての要素のピクセルのcからの距離が
                                -- 最近傍のcからの距離と等しいか大きくなくてはならない.
                                ps  = filter (/=min) testData
  in case ps of
    [] -> error "empty list not supported."
    otherwise -> all (\p -> (color p /- c) >= dis) ps
