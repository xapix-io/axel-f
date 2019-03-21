(ns axel-f.operator-test
  (:require [axel-f.core :as af]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest range-operator
  (t/is (= '(0 1 2)
           ((af/compile "0:3"))))

  (t/is (= '(0 1 2)
           ((af/compile "foo[0:3]") {:foo [0 1 2 3 4]})))

  (t/is (= '((0 (1)) (1 (1 2)) (2 (1 2 3)))
           ((af/compile "MAP({_, foo[0:INC(_)]}, 0:COUNT(foo))")
            {:foo [1 2 3]}))))
