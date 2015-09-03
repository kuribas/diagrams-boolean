{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Set operations on paths.  Open lines are removed from each path
-- (even those ending in the starting point), and overlap is removed
-- from loops.

module Diagrams.TwoD.Boolean
       (loopUnion, loopDifference,
        loopIntersection, loopExclusion,
        union, difference, intersection, exclusion)
       where
import Diagrams.Prelude
import Data.Maybe
import qualified Geom2D.CubicBezier as C

fillrule :: FillRule -> C.FillRule
fillrule Winding = C.NonZero
fillrule EvenOdd = C.EvenOdd

loop2path :: Located (Trail' Loop V2 Double) -> C.ClosedPath Double
loop2path t =
  C.ClosedPath $ go x0 y0 (lineSegments $ cutLoop $ unLoc t)
  where
    (P (V2 x0 y0)) = loc t
    go :: Double -> Double -> [Segment Closed V2 Double] -> [(C.DPoint, C.PathJoin Double)]
    go _ _ [] = []
    go x y (Linear (OffsetClosed (V2 x3 y3)):r) =
      (C.Point x y, C.JoinLine) :
      go (x+x3) (y+y3) r
    go x y (Cubic (V2 x1 y1) (V2 x2 y2) (OffsetClosed (V2 x3 y3)):r) =
      (C.Point x y, C.JoinCurve (C.Point (x+x1) (y+y1)) (C.Point (x+x2) (y+y2))) :
      go (x+x3) (y+y3) r

path2loop :: C.ClosedPath Double -> Located (Trail' Loop V2 Double)
path2loop (C.ClosedPath []) = fromSegments [] `at` origin
path2loop (C.ClosedPath ((C.Point x0 y0, join):r)) =
  fromSegments (go x0 y0 join r) `at` P (V2 x0 y0)
  where go x y C.JoinLine [] =
          [straight (V2 (x0-x) (y0-y))]
        go x y C.JoinLine ((C.Point x2 y2, join'):r') =
          straight (V2 (x2-x) (y2-y)):
          go x2 y2 join' r'
        go x y (C.JoinCurve (C.Point x1 y1) (C.Point x2 y2)) r' =
          case r' of
           [] -> [bezier3 (V2 (x1-x) (y1-y))
                  (V2 (x2-x) (y2-y)) (V2 (x0-x) (y0-y))]
           ((C.Point x3 y3, join'):r'') ->
             bezier3 (V2 (x1-x) (y1-y)) (V2 (x2-x) (y2-y))
             (V2 (x3-x) (y3-y)) :
             go x3 y3 join' r''

trail2loop :: Located (Trail V2 Double) -> Maybe (Located (Trail' Loop V2 Double))
trail2loop = located (withTrail (const Nothing) Just)

loop2trail :: Located (Trail' Loop V2 Double) -> Located (Trail V2 Double)
loop2trail = over located wrapLoop

-- | Union of a list of loops, by removing overlap.
loopUnion :: [Located (Trail' Loop V2 Double)]
          -> FillRule -> Double
          -> [Located (Trail' Loop V2 Double)]
loopUnion p fill tol =
  map path2loop $ C.union (map loop2path p) (fillrule fill) tol

-- | Difference between loops.  The loops in each lists are first merged using `union`.
loopDifference :: [Located (Trail' Loop V2 Double)]
                 -> [Located (Trail' Loop V2 Double)]
                 -> FillRule -> Double 
                 -> [Located (Trail' Loop V2 Double)]
loopDifference p1 p2 fill tol =
  map path2loop $ C.difference (map loop2path p1)
  (map loop2path p2) (fillrule fill) tol

-- | Intersection of loops.  The loops in each lists are first merged using `union`.
loopIntersection :: [Located (Trail' Loop V2 Double)]
                   -> [Located (Trail' Loop V2 Double)]
                   -> FillRule -> Double
                   -> [Located (Trail' Loop V2 Double)]
loopIntersection p1 p2 fill tol =
  map path2loop $ C.intersection (map loop2path p1)
  (map loop2path p2) (fillrule fill) tol

-- | Exclusion (xor) of loops. The loops in each lists are first merged using `union`.
loopExclusion :: [Located (Trail' Loop V2 Double)]
                -> [Located (Trail' Loop V2 Double)]
                -> FillRule -> Double
                -> [Located (Trail' Loop V2 Double)]
loopExclusion p1 p2 fill tol =
  map path2loop $ C.exclusion (map loop2path p1)
  (map loop2path p2) (fillrule fill) tol

-- | Remove overlapping regions in the path. 
union :: (ToPath t, N t ~ Double, V t ~ V2) =>
         t -> FillRule -> Double -> Path V2 Double
union p fill tol =
  Path $ map loop2trail $ 
     loopUnion (mapMaybe trail2loop $ pathTrails (toPath p)) fill tol

-- | Intersection of two paths.
intersection :: (ToPath t1, ToPath t, N t1 ~ Double, N t ~ Double,
                 V t1 ~ V2, V t ~ V2) =>
                t -> t1 -> FillRule -> Double -> Path V2 Double
intersection p1 p2 fill tol =
  Path $ map loop2trail $
  loopIntersection
  (mapMaybe trail2loop $ pathTrails (toPath p1))
  (mapMaybe trail2loop $ pathTrails (toPath p2))
  fill
  tol

-- | difference of two paths.
difference :: (ToPath t1, ToPath t, N t1 ~ Double, N t ~ Double,
               V t1 ~ V2, V t ~ V2) =>
              t -> t1 -> FillRule -> Double -> Path V2 Double
difference p1 p2 fill tol =
  Path $ map loop2trail $
  loopDifference
  (mapMaybe trail2loop $ pathTrails (toPath p1))
  (mapMaybe trail2loop $ pathTrails (toPath p2))
  fill tol

-- | Exclusion (exclusive or) of two paths.
exclusion :: (ToPath t1, ToPath t, N t1 ~ Double, N t ~ Double,
              V t1 ~ V2, V t ~ V2) =>
             t -> t1 -> FillRule -> Double -> Path V2 Double
exclusion p1 p2 fill tol =
  Path $ map loop2trail $
  loopExclusion
  (mapMaybe trail2loop $ pathTrails (toPath p1))
  (mapMaybe trail2loop $ pathTrails (toPath p2))
  fill tol

