(ns io.zane.plot.compress.heatmap
  (:require [clojure.core.matrix :as matrix]
            [thi.ng.color.core :as color]
            [io.zane.plot.ansi :as ansi]
            [io.zane.plot.compress :as compress]))

(def densities
  [\space \░ \▒ \▓ \█])

(defn character
  "Takes a two-dimensional matrix of colors and produces from it the corresponding
  heatmap character."
  [m]
  (let [maximum (reduce * (matrix/shape m))
        amount (transduce (comp cat
                                (map #(if % 1 0)))
                          +
                          m)
        index (* (/ amount maximum)
                 (dec (count densities)))]
    (nth densities index)))

(def color
  "Returns a single color representing the blend of all the colors in the
  two-dimensional matrix."
  (constantly ::ansi/default))

(defn compress
  "Returns a two-dimensional matrix of characters where each character is shaded
  according to the values in the corresponding section of the source matrix."
  [m]
  (let [width 2
        height 4]
    (compress/compress-cells
     m
     height
     width
     character
     color)))
