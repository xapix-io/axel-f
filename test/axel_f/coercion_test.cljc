(ns axel-f.coercion-test
  (:require  [axel-f.functions.coercion :as sut]
             #?(:clj [clojure.test :as t]
                :cljs [cljs.test :as t :include-macros true])))

(t/deftest excel-number-test

  (t/testing "numbers coerced correctly"

    (t/are [x] (= x (sut/excel-number x))
      1
      1.2
      1.e3))

  (t/testing "numbers as a strings coerced correctly"

    (t/are [x y] (= x (sut/excel-number y))
      1 "1"
      1.2 "1.2"
      1.e3 "1.e3"))

  (t/testing "booleans coerced correctly"

    (t/are [x y] (= x (sut/excel-number y))
      1 true
      0 false)))

(t/deftest excel-str-test

  (t/testing "booleans coerced to strings correctly"

    (t/are [x y] (= x (sut/excel-str y))
      "TRUE" true
      "FALSE" false))

  (t/testing "str function applied for any other objects"

    (t/are [x] (= (str x) (sut/excel-str x))
      {:foo 1}
      "bar"
      1
      :bar
      [1 2 3])))
