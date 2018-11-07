(ns axel-f.autocomplete-test
  (:require [axel-f.autocomplete :as sut]
            [axel-f.macros :refer [functions-store]]
            axel-f.functions
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest autocomplete-test
  (t/testing "autocomplete function return empty vector for unbalanced quotes"
    (t/are [x] (= [] (sut/autocomplete x))
      "\""
      "'"
      "CONCATENATE('"
      "CONCATENATE(\""))

  (t/testing "autocomplete function return array of suggestions for incomplete formula with"
    (with-redefs [functions-store (atom {"SUM"   {}
                                         "ROUND" {}
                                         "MIN"   {}
                                         "MAX"   {}})]
      (t/is (= [{:type :FN, :value "SUM"}
                {:type :FN, :value "ROUND"}
                {:type :FN, :value "MIN"}
                {:type :FN, :value "MAX"}]
               (sut/autocomplete "" {})))

      (t/testing "begining of the function"
        (t/is (= [{:type :FN :value "SUM"}]
                 (sut/autocomplete "SU")))

        (t/is (= [{:type :FN :value "ROUND"}]
                 (sut/autocomplete "ROU")))

        (t/is (= [{:type :FN :value "MIN"} {:type :FN :value "MAX"}]
                 (sut/autocomplete "M")))))

    (t/testing "fuzzy match"
      (t/is (= [{:type  :FN
                 :value "MID"
                 :doc  "Returns a segment of a string."}
                {:type  :FN
                 :value "MIN"
                 :doc  "Returns the minimum value in a numeric dataset."}
                {:type  :FN
                 :value "MAX"
                 :doc  "Returns the maximum value in a numeric dataset."}]
               (sut/autocomplete "MIX")))

      (t/is (= [{:type  :FN
                 :value "MID"
                 :doc  "Returns a segment of a string."}
                {:type  :FN
                 :value "MIN"
                 :doc  "Returns the minimum value in a numeric dataset."}
                {:type  :FN
                 :value "TRIM"
                 :doc  "Removes leading, trailing, and repeated spaces in text."}
                {:type  :OBJREF
                 :value "mio"
                 :doc  "Field in the context"}]
               (sut/autocomplete "MI" {:mio 1}))))

    (t/testing "function call with incomplete list of arguments"
      (t/is (= [{:type :FNCALL
                 :value "SUM"
                 :current-arg 0
                 :doc "Returns the sum of a series of numbers and/or references."}]
               (sut/autocomplete "SUM(")))

      (t/is (= [{:type :FNCALL
                 :value "SUM"
                 :current-arg 1
                 :doc "Returns the sum of a series of numbers and/or references."}]
               (sut/autocomplete "SUM(0, 1")))

      (t/is (= [{:type :FNCALL
                 :value "SUM"
                 :current-arg 2
                 :doc "Returns the sum of a series of numbers and/or references."}]
               (sut/autocomplete "SUM(-1,1,  5")))

      (t/is (= [{:type :FNCALL
                 :value "SUM"
                 :current-arg 2
                 :doc "Returns the sum of a series of numbers and/or references."}]
               (sut/autocomplete "MIN(SUM(-1,1,  5")))

      (t/is (= [{:type :FNCALL
                 :value "MIN"
                 :current-arg 0
                 :doc "Returns the minimum value in a numeric dataset."}]
               (sut/autocomplete "MIN(SUM(-1,1,  5)")))

      (t/is (= [{:type :FNCALL
                 :value "MIN"
                 :current-arg 1
                 :doc "Returns the minimum value in a numeric dataset."}]
               (sut/autocomplete "MIN(1, SUM(-1,1,  5)")))

      (t/is (= [{:type :FNCALL
                 :value "MIN"
                 :current-arg 1
                 :doc "Returns the minimum value in a numeric dataset."}]
               (sut/autocomplete "MAX(MIN(1, SUM(-1,1,  5)")))

      (t/is (= [{:type :FNCALL
                 :value "MIN"
                 :current-arg 1
                 :doc "Returns the minimum value in a numeric dataset."}]
               (sut/autocomplete "MAX(MIN(1, SUM(-1,1,  5)")))

      (t/is (= [{:type :FNCALL
                 :value "MIN"
                 :current-arg 1
                 :doc "Returns the minimum value in a numeric dataset."}]
               (sut/autocomplete "MIN(SUM(-1,1,  5), \"FOO\""))))

    (t/testing "function call with reference as a last argument"
      (t/is (= [{:type :OBJREF :value "foo"}]
               (map #(select-keys % [:type :value])
                    (sut/autocomplete "SUM(foo" {:foo {:bar {:bar 1 :baz 2}}}))))

      (t/is (= [{:type :OBJREF :value "bar"}]
               (map #(select-keys % [:type :value])
                    (sut/autocomplete "SUM(foo." {:foo {:bar {:bar 1 :baz 2}}}))))

      (t/is (= [{:type :OBJREF :value "bar"}]
               (map #(select-keys % [:type :value])
                    (sut/autocomplete "SUM(foo.bar" {:foo {:bar {:bar 1 :baz 2}}}))))

      (t/is (= [{:type :OBJREF :value "bar"}]
               (map #(select-keys % [:type :value])
                    (sut/autocomplete "SUM(1, foo.bar" {:foo {:bar {:bar 1 :baz 2}}}))))

      (t/is (= [{:type :OBJREF :value "bar"} {:type :OBJREF :value "baz"}]
               (map #(select-keys % [:type :value])
                    (sut/autocomplete "SUM(foo.bar." {:foo {:bar {:bar 1 :baz 2}}}))))

      (t/is (= [{:type :OBJREF :value "bar"}
                {:type :OBJREF :value "baz"}
                {:type :OBJREF :value "boom"}]
               (map #(select-keys % [:type :value])
                    (sut/autocomplete "foo[*].b" {:foo [{:bar 1 "baz" 2}
                                                        {:bar 4 :baz 1 :boom 7}]}))))

      (t/is (= []
               (map #(select-keys % [:type :value])
                    (sut/autocomplete "foo[*].b" {:foo [1 2]}))))

      (t/is (= [{:type :OBJREF :value "bar"}]
               (map #(select-keys % [:type :value])
                    (sut/autocomplete "foo[*]" {:foo [{:bar 1} {:bar 2}]}))))

      (t/is (= []
               (map #(select-keys % [:type :value])
                    (sut/autocomplete "foo[0]" {:foo [{:bar 1} {:bar 2}]}))))

      (t/is (= [{:type :OBJREF :value "bar"}]
               (map #(select-keys % [:type :value])
                    (sut/autocomplete "foo[0]." {:foo [{:bar 1} {:bar 2}]})))))))
