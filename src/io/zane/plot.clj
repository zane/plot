(ns io.zane.plot
  (:require [io.zane.plot.axes :as axes]
            [io.zane.plot.border :as border]
            [io.zane.plot.canvas :as canvas]
            [io.zane.plot.matrix :as matrix]
            [io.zane.plot.compress :as compress]
            [io.zane.plot.compress.heatmap :as heatmap]
            [io.zane.plot.compress.braille :as braille]))

(defn draw
  "Generates a plot from the provided data."
  ([pts compress]
   (plot pts compress {}))
  ([pts compress options]
   (let [defaults {:width 40 :height 20}
         {:keys [width height]} (merge defaults options)
         canvas (matrix/fill (constantly false)
                             (* width braille/cell-width)
                             (* height braille/cell-height))
         [x-min x-max] (matrix/extremes (map first pts))
         [y-min y-max] (matrix/extremes (map second pts))]
     (-> canvas
         (canvas/points {:origin-x x-min
                         :origin-y y-min
                         :width (- x-max x-min)
                         :height (- y-max y-min)}
                        pts)
         (compress)
         (border/draw)
         (axes/draw x-min x-max y-min y-max)
         (matrix/string)
         (print)))))
