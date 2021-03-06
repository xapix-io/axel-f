(ns axel-f.excel.geo)

(def earth-radius 6371.0)

(defn to-radians
  "Converts an angle measured in degrees to an approximately
  equivalent angle measured in radians."
  [x]
  #?(:clj (Math/toRadians x)
     :cljs (* x (/ Math/PI 180))))

(defn asin
  "Returns the arc sine of `x`."
  [x]
  (Math/asin x))

(defn sin
  "Returns the sine of `x`."
  [x]
  (Math/sin x))

(defn cos
  "Returns the cosine of `x`."
  [x]
  (Math/cos x))

(defn sqrt
  "Returns the square root of `x`."
  [x]
  (Math/sqrt x))

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

(defn geo-distance*
  "Calculate the distance for the path described as a list of geo points. Each point must a tuple of two float numbers."
  [& ^{:doc "Collection of coordinates"} points]
  (->> points
       flatten
       (partition 2)
       (partition 2 1)
       (reduce #(+ %1 (apply distance %2)) 0)))

(def geo-distance #'geo-distance*)

(def env
  {"GEO" {"DISTANCE" geo-distance}})
