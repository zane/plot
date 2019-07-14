(ns io.zane.plot.compress
  (:require [io.zane.plot.ansi :as ansi]))

(defn column-partition
  "Returns a lazy sequence of two-dimensional arrays from the provided two
  dimensional array. The arrays will be of width n. The height of the arrays
  will be the same as the height of the input array."
  [n m]
  (when (seq (first m))
    (lazy-seq (cons (map #(take n %)
                         m)
                    (column-partition n (map #(drop n %)
                                             m))))))

(defn matrix-str
  [char-f color-f m]
  (let [c (char-f m)
        color (color-f m)
        code (ansi/closest-code color)]
    (str (ansi/escape-code code)
         c
         (ansi/escape-code ansi/reset))))

(defn compress-cells
  "Takes a two-dimensional matrix of colors and compresses it into a smaller
  matrix of ANSI-styled one-character strings using the function provided. Both
  functions are expected to take a matrix."
  [m height width char-f color-f]
  (->> m
       (partition height)
       (map #(column-partition width %))
       (map #(map (partial matrix-str char-f color-f)
                  %))))
