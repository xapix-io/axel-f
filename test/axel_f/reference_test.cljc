(ns axel-f.reference-test
  (:require [axel-f.excel :as af]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest list-refs
  (t/is (= '(1 2 3)
           ((af/compile "_.[*]") [1 2 3])))

  (t/is (= 2
           ((af/compile "_.[1 + 0]") [1 2 3]))))

(t/deftest reference-can-start-with-string
  (t/is (= 1
           ((af/compile "'foo'.bar") {:foo {:bar 1}}))))
