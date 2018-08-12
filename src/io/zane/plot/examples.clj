(ns io.zane.plot.examples
  (:require [clojure.math.numeric-tower :as math]
            [incanter.stats :as stats]
            [io.zane.plot :as plot]
            [io.zane.plot.compress.braille :as braille]
            [io.zane.plot.compress.heatmap :as heatmap]))

(defn trig
  []
  (plot/draw-functions braille/compress 
                       (range -2 14 1/100)
                       #(Math/cos %)
                       #(Math/sin %)
                       #(- (/ % 8)
                           1)))

(defn pow
  []
  (plot/draw-functions braille/compress
                       (range 0 10 1/25)
                       #(math/expt % 1.5)
                       #(math/expt % 2)
                       #(math/expt % 2.5)))

(defn scatter
  []
  (let [size 500]
    (plot/draw (map vector
                    (stats/sample-normal size)
                    (stats/sample-normal size))
               braille/compress)))

(defn heatmap
  []
  (let [size 1000]
    (plot/draw (map vector
                    (stats/sample-normal size)
                    (stats/sample-normal size))
               heatmap/compress)))

(defn corners
  []
  (plot/draw (for [x [0 1]
                   y [0 1]]
               [x y])
             braille/compress
             {:width 1
              :height 1}))

(defn all
  []
  (doseq [plot [trig pow scatter heatmap corners]]
    (plot)))

#_(trig)
#_(pow)
#_(scatter)
#_(heatmap)
#_(corners)
#_(all)
