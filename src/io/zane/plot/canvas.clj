(ns io.zane.plot.canvas
  (:require [clojure.core.matrix :as matrix]
            [clojure.math.numeric-tower :as math]
            [io.zane.plot.ansi :as ansi]
            [io.zane.plot.matrix :as plot.matrix]))

(defn cell-width
  [{window-width :width} canvas]
  (let [[_ canvas-width] (matrix/shape canvas)]
    (/ window-width canvas-width)))

(defn cell-height
  [{window-height :height} canvas]
  (let [[canvas-height _] (matrix/shape canvas)]
    (/ canvas-height canvas-height)))

(defn pixel-index
  [pixel-count origin pixel-width n]
  (let [distance-from-origin (- n origin)]
    (math/round
     (* (/ distance-from-origin pixel-width)
        (dec pixel-count)))))

(defn point
  "Returns the canvas with a point at the provided coordinates."
  ([window canvas x y]
   (point window canvas x y ::ansi/default))
  ([{:keys [origin-x origin-y width height] :as window} canvas x y color]
   (let [[cell-height cell-width] (matrix/shape canvas)
         x-index (pixel-index cell-width origin-x width x)
         y-index (- (dec cell-height)
                    (pixel-index cell-height origin-y height y))]
     (if-not (plot.matrix/in-bounds? canvas [y-index x-index])
       canvas
       (update-in canvas [y-index x-index] ansi/mix color)))))

(defn points
  "Returns the canvas with points at the provided coordinates."
  ([canvas window ps]
   (points canvas window ps ::ansi/default))
  ([canvas window ps color]
   (reduce (fn [canvas [x y]]
             (point window canvas x y color))
           canvas
           ps)))
