{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Set operations on paths.  Only loops are used, lines are removed
-- from each path before each operation.
module Diagrams.TwoD.Boolean
       (union, difference, intersection, exclusion)
       where
import Diagrams.Prelude
import Data.Maybe
import qualified Geom2D.CubicBezier as C

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

path2trail :: C.ClosedPath Double -> Maybe (Located (Trail V2 Double))
path2trail (C.ClosedPath []) = Nothing
path2trail (C.ClosedPath ((C.Point x y, join):r)) =
  Just $ fromSegments (go 0 0 join r) `at` P (V2 x y)
  where go x y C.JoinLine [] =
          [straight (V2 (-x) (-y))]
        go x1 y1 C.JoinLine ((C.Point x2 y2, join'):r') =
          straight (V2 (x2-x1) (y2-y1)):
          go x2 y2 join' r'
        go x y (C.JoinCurve (C.Point x1 y1) (C.Point x2 y2)) r' =
          case r' of
           [] -> [bezier3 (V2 (x1-x) (y1-x))
                  (V2 (x2-x) (y2-y)) (V2 (-x) (-y))]
           ((C.Point x3 y3, join'):r'') ->
             bezier3 (V2 (x1-x) (y1-x)) (V2 (x2-x) (y2-y))
             (V2 (x3-x) (y3-y)) :
             go x3 y3 join' r''

trail2path :: Located (Trail V2 Double) -> Maybe (C.ClosedPath Double)
trail2path =
  fmap loop2path .
  located (withTrail (const Nothing) Just)

trailsUnion :: [Located (Trail V2 Double)]
            -> Double -> [Located (Trail V2 Double)]
trailsUnion p tol =
  mapMaybe path2trail $ C.union (mapMaybe trail2path p) tol

trailsDifference :: [Located (Trail V2 Double)]
                 -> [Located (Trail V2 Double)]
                 -> Double
                 -> [Located (Trail V2 Double)]
trailsDifference p1 p2 tol =
  mapMaybe path2trail $ C.difference (mapMaybe trail2path p1)
  (mapMaybe trail2path p2) tol

trailsIntersection :: [Located (Trail V2 Double)]
                   -> [Located (Trail V2 Double)]
                   -> Double
                   -> [Located (Trail V2 Double)]
trailsIntersection p1 p2 tol =
  mapMaybe path2trail $ C.intersection (mapMaybe trail2path p1)
  (mapMaybe trail2path p2) tol

trailsExclusion :: [Located (Trail V2 Double)]
                -> [Located (Trail V2 Double)]
                -> Double
                -> [Located (Trail V2 Double)]
trailsExclusion p1 p2 tol =
  mapMaybe path2trail $ C.exclusion (mapMaybe trail2path p1)
  (mapMaybe trail2path p2) tol

-- | Remove overlapping regions in the path.
union :: (ToPath t, N t ~ Double, V t ~ V2) =>
         t -> Double -> Path V2 Double
union p tol =
  Path $ trailsUnion (pathTrails (toPath p)) tol

-- | Intersection of two paths.
intersection :: (ToPath t1, ToPath t, N t1 ~ Double, N t ~ Double,
                 V t1 ~ V2, V t ~ V2) =>
                t -> t1 -> Double -> Path V2 Double
intersection p1 p2 tol =
  Path $ trailsIntersection (pathTrails (toPath p1))
  (pathTrails (toPath p2)) tol

-- | difference of two paths.
difference :: (ToPath t1, ToPath t, N t1 ~ Double, N t ~ Double,
               V t1 ~ V2, V t ~ V2) =>
              t -> t1 -> Double -> Path V2 Double
difference p1 p2 tol =
  Path $ trailsDifference (pathTrails (toPath p1))
  (pathTrails (toPath p2)) tol

-- | Exclusion (exclusive or) of two paths.
exclusion :: (ToPath t1, ToPath t, N t1 ~ Double, N t ~ Double,
              V t1 ~ V2, V t ~ V2) =>
             t -> t1 -> Double -> Path V2 Double
exclusion p1 p2 tol =
  Path $ trailsExclusion (pathTrails (toPath p1))
  (pathTrails (toPath p2)) tol

