(ns io.zane.plot.axes
  (:require [clojure.core.matrix :as matrix]
            [io.zane.plot.matrix :as plot.matrix]))

(defn text
  "Write a string onto a character matrix starting at the provided point and
  proceeding in the provided direction."
  [m s x y direction]
  (let [step (case direction :left dec, :right inc, :up inc, :down dec)
        dimension (case direction :left 1, :right 1, :up 0, :down 0)
        [height width] (matrix/shape m)]
    (loop [m m
           s (cond-> s (contains? #{:left :up} direction) reverse)
           point [y x]]
      (if-not (and (plot.matrix/in-bounds? m point)
                   (seq s))
        m
        (recur (assoc-in m point (first s))
               (rest s)
               (update point dimension step))))))

(defn x-axis
  "Generates a y-axis from labels."
  [width x-min x-max formatter]
  (let [x-min-str (formatter x-min)
        x-max-str (formatter x-max)
        height 1]
    (-> (plot.matrix/fill (constantly \space) width height)
        (text x-min-str (inc 0) 0 :right)
        (text x-max-str (- width 2) 0 :left))))

(defn y-axis
  "Generates a y-axis from labels."
  [height y-min y-max formatter]
  (let [y-min-str (formatter y-min)
        y-max-str (formatter y-max)
        width (reduce max (map count [y-min-str y-max-str]))]
    (-> (plot.matrix/fill (constantly \space)
                          width
                          height)
        (text y-max-str (dec width) 1 :left)
        (text y-min-str (dec width) (dec (dec height)) :left))))

(defn draw
  "Attaches y-axis labels to a matrix."
  [m x-min x-max y-min y-max]
  (let [formatter #(format "%.2f" (float %))
        [height width] (matrix/shape m)
        y-axis-m (y-axis height y-min y-max formatter)
        [_ y-axis-width] (matrix/shape y-axis-m)
        x-axis-m (x-axis width x-min x-max formatter)
        [x-axis-height _] (matrix/shape x-axis-m)]
    (matrix/join-along 0
                       (matrix/join-along 1 y-axis-m m)
                       (matrix/join-along 1
                                          (plot.matrix/fill (constantly \space)
                                                            y-axis-width
                                                            x-axis-height)
                                          x-axis-m))))
