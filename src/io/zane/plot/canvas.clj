(ns io.zane.plot.canvas
  (:require [clojure.core.matrix :as matrix]
            [clojure.math.numeric-tower :as math]
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
  [{:keys [origin-x origin-y width height] :as window} canvas x y]
  (let [[cell-height cell-width] (matrix/shape canvas)
        x-index (pixel-index cell-width origin-x width x)
        y-index (- (dec cell-height)
                   (pixel-index cell-height origin-y height y))]
    (if-not (plot.matrix/in-bounds? canvas [y-index x-index])
      canvas
      (assoc-in canvas [y-index x-index] true))))

(defn points
  "Returns the canvas with points at the provided coordinates."
  ([canvas window ps]
   (reduce (fn [canvas [x y]]
             (point window canvas x y))
           canvas
           ps)))


