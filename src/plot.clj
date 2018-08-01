(ns plot
  (:require [clojure.core.matrix :as matrix]
            [io.aviso.ansi :as ansi]))

(def braille
  [[\⠁ \⠈]
   [\⠂ \⠐]
   [\⠄ \⠠]
   [\⡀ \⢀]])

(def box-upper-left \┌)
(def box-upper-right \┐)
(def box-lower-left \└)
(def box-lower-right \┘)
(def box-horizontal \─)
(def box-vertical \│)

(def braille-cell-width (second (matrix/shape braille)))
(def braille-cell-height (first (matrix/shape braille)))

(defn space?
  [x]
  (= \space x))

(defn combine
  [a b]
  (cond (space? a) b
        (space? b) a
        :else (char (bit-or (int a)
                            (int b)))))

(defn fill
  [f width height]
  (let [row-f #(into [] (take width (repeatedly f)))]
    (into [] (take height (repeatedly row-f)))))

(defn to-braille
  [grid]
  (->> grid
       (map-indexed (fn [y row]
                      (map-indexed (fn [x cell]
                                     (if cell
                                       (get-in braille [y x])
                                       \space))
                                   row)))
       (mapcat identity)
       (reduce combine)))

(defn column-partition
  [n m]
  (when (seq (first m))
    (lazy-seq (cons (map #(take n %)
                         m)
                    (column-partition n (map #(drop n %)
                                             m))))))

(defn divisible?
  [x y]
  (zero? (mod x y)))

(defn compress-cells
  "Takes a two-dimensional matrix of booleans and compresses it into a smaller
  matrix using the function provided. `f` is expected to take a two-dimensional
  array with dimensions `[height width]`."
  [m height width f]
  (->> m
       (partition height)
       (map #(column-partition width %))
       (map #(map to-braille %))))

(defn compress-to-braille
  "Takes a two-dimensional matrix of booleans and compresses it into a smaller
  matrix of braille characters. The resulting matrix will be half as wide and
  one fourth as tall."
  [m]
  (compress-cells m
                  braille-cell-height
                  braille-cell-width
                  to-braille))

(defn matrix-str
  "Takes a matrix of characters and converts it into a string by adding newlines at
  the end of each row and then concatenating everything together."
  [m]
  (transduce (comp (map #(reduce str %))
                   (interpose \newline))
             str
             m))

(defn in-bounds?
  [{:keys [origin-x origin-y width height]
    :as window}
   x
   y]
  (let [max-x (+ origin-x width)
        max-y (+ origin-y height)]
    (and (< origin-x x max-x)
         (< origin-y y max-y))))

(defn cell-width
  [{window-width :width} canvas]
  (let [[_ canvas-width] (matrix/shape canvas)]
    (/ window-width canvas-width)))

(defn cell-height
  [{window-height :height} canvas]
  (let [[canvas-height _] (matrix/shape canvas)]
    (/ canvas-height canvas-height)))

(defn canvas-index
  [{:keys [origin-x origin-y width height] :as window} canvas dimension n]
  (let [cell-size (nth (matrix/shape canvas) dimension)]
    (int (Math/floor (* (/ (- n
                              (nth [origin-y origin-x] dimension))
                           (nth [height width] dimension))
                        cell-size)))))

(defn point
  [window canvas x y]
  (if-not (in-bounds? window x y)
    canvas
    (let [[cell-height cell-width] (matrix/shape canvas)
          x-index (canvas-index window canvas 1 x)
          y-index (canvas-index window canvas 0 y)]
      (assoc-in canvas [y-index x-index] true))))

(defn points
  ([canvas ps]
   (let [min-x (reduce min (map first ps))
         max-x (reduce max (map first ps))
         min-y (reduce min (map second ps))
         max-y (reduce max (map second ps))]
     (points {:origin-x min-x
              :origin-y min-y
              :width (- max-x min-x)
              :height (- max-y min-y)}
             canvas
             ps)))
  ([window canvas ps]
   (reduce (fn [canvas [x y]]
             (point window canvas x y))
           canvas
           ps)))

(defn box-around
  [m]
  (let [[height width] (matrix/shape m)
        top [(vec (take width (repeat box-horizontal)))]
        bottom top
        box-center (repeat height box-vertical)
        left-wall (matrix/transpose
                   [(concat [box-upper-left]
                            box-center
                            [box-lower-left])])
        right-wall (matrix/transpose
                    [(concat [box-upper-right]
                             box-center
                             [box-lower-right])])]
    (matrix/join-along 1
                       left-wall
                       (matrix/join-along 0 top m bottom)
                       right-wall)))

(defn test-plot
  []
  (let [canvas (fill (constantly false) 80 40)
        xs (range -2 7 1/100)
        cos-points (for [x xs]
                     [x (float (Math/cos x))])
        sin-points (for [x xs]
                     [x (float (Math/sin x))])]
    (->> (points canvas (into cos-points sin-points))
         (compress-to-braille)
         (box-around)
         (matrix-str)
         (print))))

(defn test-scatter
  []
  (let [canvas (fill (constantly false) 80 40)
        pts (take 200 (repeatedly #(vector (rand 320)
                                           (rand 80))))]
    (->> (points canvas pts)
         (compress-to-braille)
         (box-around)
         (matrix-str)
         (print))))

#_
(test-plot)

#_
(test-scatter)
