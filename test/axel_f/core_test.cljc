(ns axel-f.core-test
  (:require [axel-f.core :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true]))
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(t/deftest run

  (t/is (= 2 (sut/run "1 + 1")))

  (t/is (= 2 (sut/run (fn [ctx] (+ 1 1)))))

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Formula must be a string or precompiled expression."
         (sut/run [:SUM [1 2 3]]))))

(t/deftest corner-cases

  (t/are [x y] (t/is (= x (sut/run y)))
    1 "+1"
    1 "--TRUE"
    0 "--FALSE"
    -1 "-TRUE"
    0 "-(1<0)"
    1 "+(1<2)"
    0 "-FALSE"
    2 "1 + 1"
    0 "1 - 1"
    "ab" "\"a\" & \"b\""
    #?(:clj 1/2
       :cljs 0.5) "1 / 2"
    4 "2 * 2"
    true "1 <> 2"
    false "1 <> 1"
    true "1 = 1"
    false "1 = 2"
    true "1 <= 2"
    true " 1 <= 1"
    false "2 <= 1"
    true "1 < 2"
    false "1 < 1"
    false "1 < 0"
    true "1 >= 0"
    true "1 >= 1"
    false "1 >= 2"
    true "1 > 0"
    false "1 > 1"
    false "1 > 2"
    true "TRUE"
    true "True"
    true "true"
    false "FALSE"
    false "False"
    false "false"
    8.0 "(1+1)^3"
    8.0 "(1+1)^SUM(1,2)"))
