(ns axel-f.operator-test
  (:require [axel-f.excel :as af]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest range-operator
  (t/is (= '(0 1 2)
           ((af/compile "0 : 3"))))

  (t/is (= '(0 1 2)
           ((af/compile "foo[0 : 3]") {:foo [0 1 2 3 4]})))

  (t/is (= '((0 (1)) (1 (1 2)) (2 (1 2 3)))
           ((af/compile "MAP(FN(x, {x, foo[0 : INC(x)]}), 0 : COUNT(foo))")
            {:foo [1 2 3]}))))

(t/deftest prefix
  (t/is (= false
           ((af/compile "!TRUE"))))

  (t/is (= true
           ((af/compile "!!true"))))

  (t/is (= 1
           ((af/compile "+1"))))

  (t/is (= -1
           ((af/compile "-1"))))

  (t/is (= -1
           ((af/compile "-(+1)"))))

  (t/is (= 1
           ((af/compile "-foo.bar") {:foo {:bar -1}}))))

(t/deftest postfix
  (t/is (= 0.02
           ((af/compile "2%"))))

  (t/is (= -0.02
           ((af/compile "-2%")))))
