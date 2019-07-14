(ns io.zane.plot.compress.braille
  (:require [clojure.core.matrix :as matrix]
            [io.zane.plot.ansi :as ansi]
            [io.zane.plot.compress :as compress]))

(def blank \⠀)

(def pattern
  [[\⠁ \⠈]
   [\⠂ \⠐]
   [\⠄ \⠠]
   [\⡀ \⢀]])

(def cell-width (second (matrix/shape pattern)))
(def cell-height (first (matrix/shape pattern)))

(defn union
  "Takes braille characters or \\space and combines them in such a way that the
  set of dots on the resulting character is the union of the dots on the input
  characters."
  ([a]
   a)
  ([a b]
   (cond (= blank a) b
         (= blank b) a
         :else (char (bit-or (int a)
                             (int b))))))

(defn character
  "Takes a two-dimensional width 2 height 4 array of colors and produces from it
  the corresponding braille character."
  [grid]
  (transduce (comp (map-indexed
                    (fn [y row]
                      (map-indexed
                       (fn [x cell]
                         (if cell
                           (get-in pattern [y x])
                           blank))
                       row)))
                   cat)
             union
             blank
             grid))

(defn color
  "Returns a color representing the blend of all the colors in matrix m."
  [m]
  (transduce cat
             (completing ansi/mix)
             ::ansi/default
             m))

(defn compress
  "Takes a two-dimensional matrix of booleans and compresses it into a smaller
  matrix of braille characters. The resulting matrix will be half as wide and
  one fourth as tall."
  [m]
  (compress/compress-cells m
                           cell-height
                           cell-width
                           character
                           color))
