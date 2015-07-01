{-# OPTIONS_GHC -Wall -O0 #-}

module Main where

import Vis

import Linear
import Control.Lens

import Data.Function (on)
import Data.List ( sortBy )
import System.Environment

import qualified Data.Map.Strict as M

import qualified Data.Tree as T
import Control.Monad ( guard )

main :: IO ()
main = do
  argv <- getArgs
  animate 
   (defaultOpts { optWindowName = "animate test"
           , optInitialCamera = Just $ Camera0 0 (-60) 15
        })
   $ \ time ->
    let quat x = normalize
          $ Quaternion 1 (V3 x 0 0 )
    in  RotQuat (quat 0.5)
        -- $ Trans (V3 (-10) 0 (-10))
        $  case argv of
    [ "1" ] -> VisObjects [ plane
         , rule_applications_for time ex1i mx1
           $ cube_points 3
           ]
    [ "2" ] -> VisObjects [ plane
         , reachable time cx1 4
           ]
    [ "3" ] -> VisObjects [ plane
         , rule_applications_for time ex1i mx1
           $ reachable_points cx1  4
         , reachable time cx1 4
           ]
    _ -> error "huh"

drawFun :: VisObject Double
drawFun  = VisObjects $
      [ plane
      -- , rule_applications_for z002 m002 $ cube_points 2
      -- , rule_applications_for ex1i mx1 $ square_points 3
      -- , reachable cx1
      -- , rule_applications_for ex1i mx1 $ reachable_points cx1  2
      ]
    


data Linear = Linear (V3 Double) (M33 Double)
  deriving Show

apply :: Linear -> V3 Double -> V3 Double
apply (Linear a m) p = a + m !* p

applies ::  (M.Map Char Linear) -> String
        -> V3 Double -> V3 Double
applies int s p =
  foldr (\ c p -> apply (int M.! c) p) p s

data Cone = Cone (V3 Double) (M.Map Char Linear)
  deriving Show

-- (RULES a a -> a b a , b b -> a)
zabba = [("aa","aba"),("bb","a")]
mabba = M.fromList
  [('a', Linear (V3 0 0 1)(V3(V3 1 0 3)(V3 0 0 0)(V3 0 0 0)))
  ,('b', Linear (V3 0 1 0)(V3(V3 1 2 1)(V3 0 0 1)(V3 0 1 0)))
  ]

z026 = [("abb","baa"),("aab","bba")]
m026 = M.fromList
  [ ('a', Linear (V3 1 0 0)
           (V3 (V3 1 1 0)(V3 0 0 1) (V3 0 1 1)) )
  , ('b', Linear (V3 0 1 0)
          (V3 (V3 1 1 0)(V3 0 0 1)(V3 0 1 1) ) )
  ]

c026 = Cone (V3 0 0 0) m026

z002 = [("bca","abab"),("b","cc"),("aa","acba")]
m002 = M.fromList
  [ ('a', Linear (V3 0 0 2)
          (V3 (V3 1 0 1) (V3 0 1 0) (V3 0 2 0)))
  , ('b', Linear (V3 1 0 0)
          (V3 (V3 1 1 0) (V3  0 0 0)(V3  0 1 0)))
  , ('c', Linear (V3 0 0 0)
          (V3 (V3 1 0 0) (V3 0 0 1) (V3 0 0 0)))
  ]

c002 = Cone (V3 0 0 0) m002

{- actual example from RTA 15 paper

Prove termination of $R = \{f g \to f f, gf \to gg\}$.

Use domain $D = \{(x_1,x_2,x_3)\in\NN^3\mid x_3\ge x_2+1\}$.

{}[f](x_1,x_2,x_3) &=& (x_1+2x_2+1,0,x_3+1) \\
{}[g](x_1,x_2,x_3) &=& (x_1\phantom{+2x_2+1},x_3,x_3+1) \\[5pt]
{}[fg](x)&=&(x_1+\phantom{\fbox{0}x_2+}2x_3+\fbox{1},0,x_3+2), \\
{}[ff](x)&=&(x_1+\fbox{2}x_2+\phantom{0x_2+}\fbox{2},0,x_3+2).
-}

ex1 = [("fg","ff"),("gf","gg")]
ex1i = take 1 ex1

mx1 = M.fromList
  [('f',Linear(V3 1 0 1)
        (V3 (V3 1 2 0)(V3 0 0 0) (V3  0 0 1)))
  ,('g',Linear(V3 0 0 1)
        (V3 (V3 1 0 0)(V3 0 0 1)(V3 0 0 1)))
  ]

cx1 = Cone (V3 0 0 1) mx1

f = mx1 M.! 'f'
g = mx1 M.! 'g'
Cone e _ = cx1


-- *

tree (Cone base lin) dep =
  let work s p 0 = T.Node (s,p) []
      work s p d | d > 0 =
        T.Node (s,p) $ do
          (c,f) <- M.toList lin
          return $ work (c:s) (apply f p) (d-1)
  in  work "" base dep

levelorder t =
  let lo = t : ( lo >>= T.subForest )
  in  take (length $ T.flatten t)  lo

render root = Trans (snd $ T.rootLabel root) (Sphere 0.15 Solid white) : do
  -- Text3d s p TimesRoman24 white :
    t <- levelorder root
    let (l,from) = T.rootLabel t
    (col,s) <- zip [white,greyN 0.5] $ T.subForest t
    let to = snd $ T.rootLabel s
    return $ VisObjects
      [ Line (Just 5) [from, to] col
      , Trans to $ Sphere 0.15 Solid col
      ]

cube_points d = do
  x <- [ 0 :: Int,1] -- .. d ]
  y <- [ 0 :: Int,1 .. d ]
  z <- [ 0 :: Int,1 .. d ]
  return $ V3 (realToFrac x)(realToFrac y)(realToFrac z)

square_points d = do
  let x = 0
  y <- [ 0 :: Int,1 .. d ]
  z <- [ 0 :: Int,1 .. d ]
  return $ V3 (realToFrac x)(realToFrac y)(realToFrac z)

cube d = VisObjects $ do
  [x,y,z] <- sequence $ replicate 3 [0,1]
  let p = V3 x y z
  [a,b,c] <- sequence $ replicate 3 [0,1]
  let q = V3 a b c
  guard $ 1 == qd p q
  return $ Line (Just 2) [p ,q] white

reachable_points cone dep =
   map (snd . T.rootLabel) $ levelorder $ tree cone dep
  


rule_applications_for time sys int ps = VisObjects $ do
  let candidates = do
        -- sortBy (compare `on` ( \(p,_)-> id $ p ^. _y )) 
        p <- ps ; (l,r) <- sys ; return (p, (l,r))
      n = length candidates
  let pick :: Int
      pick = mod (truncate time) n
      
  (i,(p,(l,r))) <- zip [0..] candidates
  let prepicked = i <= pick
      picked = i == pick
      line from to col =
        if picked then arrow 10 from to col
        else Line (Just 1) [from,to] col
  let from = applies int l p
      to   = applies int r p
      col = ( if not picked then transp 0.7 else id )
          $ if from ^. _x > to ^. _x then blue else red
      cola = ( if not picked then transp 0.7 else id )
          $ if from ^. _x > to ^. _x then green else red
      lw = if picked then 5 else 1
  return $ VisObjects 
    [ if picked then line from to $ light cola else blank
    , if picked then Line (Just lw) [ p, from] blue else blank
    , if picked then Line (Just lw) [ p, to  ] red else blank      
    , if picked then Triangle p from to cola else blank
    , if prepicked then Trans p $ Sphere 0.15 Solid cola else blank
    ]

blank = VisObjects []

transp f c =
  let (r,g,b,a) = rgbaOfColor c
  in makeColor r g b $ f * a

arrow ratio from to col =
  let dist = to - from
      rat = ratio * norm dist
  in  Trans from $ Arrow (norm dist , rat) dist col



axes = Axes (1,20)

reachable time cone dep =
  VisObjects $ reveal time $ render $ tree cone dep

reveal time obs =
  let n = length obs
      t = mod (truncate time) n
  in  take (succ t) obs

plane = Plane (V3 1 0 0) (greyN 0.8)
  (makeColor 0.4 0.6 0.65 0.4)





{-
    

    sphere = Trans (V3 0 x (-1)) $ Sphere 0.15 Wireframe (makeColor 0.2 0.3 0.8 1)

    
    ellipsoid = Trans (V3 x 0 (-1)) $ RotQuat quat $ Ellipsoid (0.2, 0.3, 0.4) Solid (makeColor 1 0.3 0.5 1)
    box = Trans (V3 0 0 x) $ RotQuat quat $ Box (0.2, 0.2, 0.2) Wireframe (makeColor 0 1 1 1)
    text k = Text2d "OLOLOLOLOLO" (100,500 - k*100*x) TimesRoman24 (makeColor 0 (0.5 + x'/2) (0.5 - x'/2) 1)
      where
        x' = realToFrac $ (x + 1)/0.4*k/5
    boxText = Text3d "trololololo" (V3 0 0 (x-0.2)) TimesRoman24 (makeColor 1 0 0 1)
-}

