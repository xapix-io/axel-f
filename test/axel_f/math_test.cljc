(ns axel-f.math-test
  (:require [axel-f.core :as af]
            [axel-f.functions.math :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest INC
  (t/testing "increment number"
    (t/is (= 1 (sut/inc* 0)))
    (t/is (= 1 ((af/compile "INC(0)"))))))

(t/deftest DEC
  (t/testing "decrement number"
    (t/is (= 1 (sut/dec* 2)))
    (t/is (= 1 ((af/compile "DEC(2)"))))))
