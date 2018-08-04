(ns io.zane.plot.compress.heatmap
  (:require [io.zane.plot.compress :as compress]))

(def densities
  [\space \░ \▒ \▓ \█])

(defn character
  [maximum amount]
  (let [index (* (/ amount maximum)
                 (dec (count densities)))]
    (nth densities index)))

(defn compress
  "Returns a two-dimensional matrix of characters where each character is shaded
  according to the values in the corresponding section of the source matrix."
  [m]
  (let [width 2
        height 4
        max-density (* width height)]
    (compress/compress-cells
     m
     height
     width
     (fn [a]
       (let [amount (transduce (comp cat
                                     (map #(if % 1 0)))
                               +
                               a)]
         (character max-density amount))))))
