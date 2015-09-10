{-# LANGUAGE NoMonomorphismRestriction #-}
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Boolean

main = mainWith (example :: Diagram B)

example = strokePath $ union Winding $
          (square 1) <> circle 0.5 # translate (V2 0.5 (-0.5))

