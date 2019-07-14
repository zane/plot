(ns io.zane.plot.ansi
  (:require [thi.ng.color.core :as color]
            [thi.ng.math.core :as math]))

(def codes
  {::reset "[0m"

   ::bright     "[1m"
   ::blink-slow "[5m"

   ::underline     {::on "[4m" ::off "[24m"}
   ::inverse       {::on "[7m" ::off "[27m"}
   ::strikethrough {::on "[9m" ::off "[29m"}

   ::foreground {::default "[39m"
                 ::white   "[37m"
                 ::black   "[30m"
                 ::red     "[31m"
                 ::green   "[32m"
                 ::blue    "[34m"
                 ::yellow  "[33m"
                 ::magenta "[35m"
                 ::cyan    "[36m"}

   ::background {::default "[49m"
                 ::white   "[47m"
                 ::black   "[40m"
                 ::red     "[41m"
                 ::green   "[42m"
                 ::blue    "[44m"
                 ::yellow  "[43m"
                 ::magenta "[45m"
                 ::cyan    "[46m"}})

(def reset (::reset codes))

(def colors
  {::white   color/WHITE
   ::black   color/BLACK
   ::red     color/RED
   ::green   color/GREEN
   ::blue    color/BLUE
   ::yellow  color/YELLOW
   ::magenta color/MAGENTA
   ::cyan    color/CYAN})

(defn to-8bit
  [col]
  (if (= ::default col)
    ::default
    (keyword "io.zane.plot.ansi"
             (name (color/closest-hue col color/primary-hues)))))

(defn closest-code
  [col]
  (get-in codes [::foreground (to-8bit col)]))

(defn mix
  [col1 col2]
  (cond (nil? col1) col2
        (nil? col2) col1
        (= ::default col1) col2
        (= ::default col2) col1
        :else (math/mix col1 col2)))

(def escape-char "\u001b")

(defn escape-code
  [code]
  (str escape-char code))
