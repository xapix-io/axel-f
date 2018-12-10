(ns axel-f.geo-test
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [axel-f.core :as af]
            [axel-f.geo :as sut]))

(t/deftest distance
  (t/testing "Calculate distance for given path"
    (t/are [path dist] (= (af/run "GEO.DISTANCE(_)" path) dist)
      [[55.751244 37.618423] ;; Moscow
       [52.520008 13.404954] ;; Berlin
       ]
      1608.8794097160353

      [[13.736717 100.523186] ;; Bangkok
       [55.751244 37.618423] ;; Moscow
       [52.520008 13.404954] ;; Berlin
       ]
      8676.241667937587)))
