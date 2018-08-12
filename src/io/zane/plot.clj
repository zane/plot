(ns io.zane.plot
  (:require [thi.ng.color.core :as color]
            [io.zane.plot.ansi :as ansi]
            [io.zane.plot.axes :as axes]
            [io.zane.plot.border :as border]
            [io.zane.plot.canvas :as canvas]
            [io.zane.plot.matrix :as matrix]
            [io.zane.plot.compress :as compress]
            [io.zane.plot.compress.heatmap :as heatmap]
            [io.zane.plot.compress.braille :as braille]))

(defn empty-canvas
  [width height]
  (matrix/fill (constantly nil)
               (* width braille/cell-width)
               (* height braille/cell-height)))

(def function-colors
  [color/RED color/BLUE color/GREEN])

(defn background
  [m]
  (map #(map (fn [s]
               (str (ansi/escape-code (get-in ansi/codes [::ansi/background ::ansi/black]))
                    s
                    (ansi/escape-code (get-in ansi/codes [::ansi/reset]))))
             %)
       m))

(defn draw-functions
  [compress xs & fs]
  (let [ys (for [f fs]
             (map f xs))
        [x-min x-max] (matrix/extremes xs)
        [y-min y-max] (matrix/extremes (mapcat identity ys))
        canvas (reduce (fn [canvas [ys color]]
                         (canvas/points canvas
                                        {:origin-x x-min
                                         :origin-y y-min
                                         :width (- x-max x-min)
                                         :height (- y-max y-min)}
                                        (zipmap xs ys)
                                        color))
                       (empty-canvas 80 10)
                       (zipmap ys (cycle function-colors)))]
    (-> canvas
        (compress)
        (border/draw)
        (axes/draw x-min x-max y-min y-max)
        (background)
        (matrix/string)
        (println))))

(defn draw
  "Generates a plot from the provided data."
  ([pts compress]
   (draw pts compress {}))
  ([pts compress options]
   (let [defaults {:width 40 :height 20}
         {:keys [width height]} (merge defaults options)
         canvas (matrix/fill (constantly nil)
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
