(ns io.zane.plot.border
  (:require [clojure.core.matrix :as matrix]))

(def char-upper-left \┌)
(def char-upper-right \┐)
(def char-lower-left \└)
(def char-lower-right \┘)
(def char-horizontal \─)
(def char-vertical \│)

(defn draw
  "Takes a two-dimensional array of characters and surrounds it with unicode
  characters that draw a border."
  [m]
  (let [[height width] (matrix/shape m)
        top [(vec (take width (repeat char-horizontal)))]
        bottom top
        border-center (repeat height char-vertical)
        left-wall (matrix/transpose
                   [(concat [char-upper-left]
                            border-center
                            [char-lower-left])])
        right-wall (matrix/transpose
                    [(concat [char-upper-right]
                             border-center
                             [char-lower-right])])]
    (matrix/join-along 1
                       left-wall
                       (matrix/join-along 0 top m bottom)
                       right-wall)))
