(ns io.zane.plot
  (:require [incanter.stats :as stats]
            [io.zane.plot.axes :as axes]
            [io.zane.plot.border :as border]
            [io.zane.plot.canvas :as canvas]
            [io.zane.plot.matrix :as matrix]
            [io.zane.plot.compress :as compress]
            [io.zane.plot.compress.heatmap :as heatmap]
            [io.zane.plot.compress.braille :as braille]))

(defn plot
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

(defn test-sin-cos
  []
  (plot (mapcat (fn [x]
                  [[x (Math/cos x)]
                   [x (Math/sin x)]])
                (range -2 7 1/100))
        braille/compress))

(defn test-pow
  []
  (plot (for [x (range 0 10 1/25)]
          [x (float (* x x))])
        braille/compress))

(defn test-scatter
  []
  (let [size 500]
    (plot (map vector
               (stats/sample-normal size)
               (stats/sample-normal size))
          braille/compress)))

(defn test-heatmap
  []
  (let [size 1000]
    (plot (map vector
               (stats/sample-normal size)
               (stats/sample-normal size))
          heatmap/compress)))

(defn test-corners
  []
  (plot (for [x [0 1]
              y [0 1]]
          [x y])
        braille/compress
        {:width 1
         :height 1}))

#_(test-sin-cos)
#_(test-pow)
#_(test-scatter)
#_(test-heatmap)
#_(test-corners)
