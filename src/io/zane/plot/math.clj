(ns io.zane.plot.math)

(defn divisible?
  "Returns true if x capable of being divided by y without a remainder."
  [x y]
  (zero? (rem x y)))
