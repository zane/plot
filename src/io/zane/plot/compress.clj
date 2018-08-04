(ns io.zane.plot.compress)

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

(defn compress-cells
  "Takes a two-dimensional matrix of booleans and compresses it into a smaller
  matrix using the function provided. f is expected to take a two-dimensional
  array with dimensions [height width]."
  [m height width f]
  (->> m
       (partition height)
       (map #(column-partition width %))
       (map #(map f %))))
