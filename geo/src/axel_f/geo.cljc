(ns axel-f.geo
  (:require [axel-f.macros :refer [def-excel-fn]]
            #?(:cljs [goog.math :as math])))

(def earth-radius 6371.0)

(defn to-radians
  "Converts an angle measured in degrees to an approximately
  equivalent angle measured in radians."
  [x]
  #?(:clj (Math/toRadians x)
     :cljs (math/toRadians x)))

(defn asin
  "Returns the arc sine of `x`."
  [x]
  #?(:clj (Math/asin x)
     :cljs (.asin js/Math x)))

(defn sin
  "Returns the sine of `x`."
  [x]
  #?(:clj (Math/sin x)
     :cljs (.sin js/Math x)))

(defn cos
  "Returns the cosine of `x`."
  [x]
  #?(:clj (Math/cos x)
     :cljs (.cos js/Math x)))

(defn sqrt
  "Returns the square root of `x`."
  [x]
  #?(:clj (Math/sqrt x)
     :cljs (.sqrt js/Math x)))

(defn distance
  "Returns the distance from `point-1` to `point-2`, in km using the
  Haversine formula."
  [[lat-1 lon-1] [lat-2 lon-2]]
  (let [d-lat (to-radians (- lat-2 lat-1))
        d-lon (to-radians (- lon-2 lon-1))
        lat-1 (to-radians lat-1)
        lat-2 (to-radians lat-2)
        a (+ (* (sin (/ d-lat 2))
                (sin (/ d-lat 2)))
             (* (sin (/ d-lon 2))
                (sin (/ d-lon 2))
                (cos lat-1)
                (cos lat-2)))]
    (* earth-radius 2 (asin (sqrt a)))))

(def-excel-fn geo.distance
  "Calculate the distance for the path described as a list of geo points
   Each point must a tuple of two or three float numbers."
  {:args [{:desc "List of points. Each point must be a tuple of latitude and longitude"}]}
  [points]
  (->> points
       (partition 2 1)
       (reduce #(+ %1 (apply distance %2)) 0)))
