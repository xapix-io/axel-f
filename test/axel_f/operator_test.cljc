(ns axel-f.operator-test
  (:require [axel-f.excel :as af]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest range-operator
  (t/is (= '(0 1 2)
           ((af/eval "0:3"))))

  (t/is (= '(0 1 2)
           ((af/eval "foo[0:3]") {:foo [0 1 2 3 4]})))

  (t/is (= '((0 (1)) (1 (1 2)) (2 (1 2 3)))
           ((af/eval "MAP({_, foo[0:INC(_)]}, 0:COUNT(foo))")
            {:foo [1 2 3]}))))

(t/deftest prefix
  (t/is (= false
           ((af/eval "!TRUE"))))

  (t/is (= true
           ((af/eval "!!true"))))

  (t/is (= 1
           ((af/eval "+1"))))

  (t/is (= -1
           ((af/eval "-1"))))

  (t/is (= -1
           ((af/eval "-(+1)")))))

(t/deftest postfix
  (t/is (= 0.02
           ((af/eval "2%"))))

  (t/is (= -0.02
           ((af/eval "-2%")))))
