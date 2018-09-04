(ns axel-f.autocomplete-test
  (:require [axel-f.autocomplete :as sut]
            [axel-f.functions :as functions]
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

    (with-redefs [functions/functions-map {"SUM"   {}
                                           "ROUND" {}
                                           "MIN"   {}
                                           "MAX"   {}}]

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
                 :desc  "Returns a segment of a string."
                 :args  [{:desc "The string to extract a segment from."}
                         {:desc "The index from the left of arg1 from which to begin extracting. The first character in arg1 has the index 1."}
                         {:desc "The length of the segment to extract."}]}
                {:type  :FN
                 :value "MIN"
                 :desc  "Returns the minimum value in a numeric dataset."
                 :args  [{:desc "The first value or range to consider when calculating the minimum value."}
                         {:desc       "Additional values or ranges to consider when calculating the minimum value."
                          :opt        true
                          :repeatable true}]}
                {:type  :FN
                 :value "MAX"
                 :desc  "Returns the maximum value in a numeric dataset."
                 :args  [{:desc "The first value or range to consider when calculating the maximum value."}
                         {:desc       "Additional values or ranges to consider when calculating the maximum value."
                          :opt        true
                          :repeatable true}]}]
               (sut/autocomplete "MIX")))

      (t/is (= [{:type  :FN
                 :value "MID"
                 :desc  "Returns a segment of a string."
                 :args  [{:desc "The string to extract a segment from."}
                         {:desc "The index from the left of arg1 from which to begin extracting. The first character in arg1 has the index 1."}
                         {:desc "The length of the segment to extract."}]}
                {:type  :FN
                 :value "MIN"
                 :desc  "Returns the minimum value in a numeric dataset."
                 :args  [{:desc "The first value or range to consider when calculating the minimum value."}
                         {:desc       "Additional values or ranges to consider when calculating the minimum value."
                          :opt        true
                          :repeatable true}]}
                {:type  :FN
                 :value "TRIM"
                 :desc  "Removes leading, trailing, and repeated spaces in text."
                 :args  [{:desc "The text or reference to a cell containing text to be trimmed."}]}
                {:type  :OBJREF
                 :value "mio"
                 :desc  "Field in the context"}]
               (sut/autocomplete "MI" {:mio 1}))))

    (t/testing "function call with incomplete list of arguments"

      (t/is (= [{:type :FNCALL
                 :value "SUM"
                 :current-arg 0
                 :desc "Returns the sum of a series of numbers and/or references."
                 :args [{:desc "The first number or range to add together."}
                        {:desc "Additional numbers or ranges to add to arg1."
                         :opt true
                         :repeatable true}]}]
               (sut/autocomplete "SUM(")))

      (t/is (= [{:type :FNCALL
                 :value "SUM"
                 :current-arg 1
                 :desc "Returns the sum of a series of numbers and/or references."
                 :args [{:desc "The first number or range to add together."}
                        {:desc "Additional numbers or ranges to add to arg1."
                         :opt true
                         :repeatable true}]}]
               (sut/autocomplete "SUM(0, 1")))

      (t/is (= [{:type :FNCALL
                 :value "SUM"
                 :current-arg 2
                 :desc "Returns the sum of a series of numbers and/or references."
                 :args [{:desc "The first number or range to add together."}
                        {:desc "Additional numbers or ranges to add to arg1."
                         :opt true
                         :repeatable true}]}]
               (sut/autocomplete "SUM(-1,1,  5")))

      (t/is (= [{:type :FNCALL
                 :value "SUM"
                 :current-arg 2
                 :desc "Returns the sum of a series of numbers and/or references."
                 :args [{:desc "The first number or range to add together."}
                        {:desc "Additional numbers or ranges to add to arg1."
                         :opt true
                         :repeatable true}]}]
               (sut/autocomplete "MIN(SUM(-1,1,  5")))

      (t/is (= [{:type :FNCALL
                 :value "MIN"
                 :current-arg 0
                 :desc "Returns the minimum value in a numeric dataset."
                 :args [{:desc "The first value or range to consider when calculating the minimum value."}
                        {:desc "Additional values or ranges to consider when calculating the minimum value."
                         :opt true
                         :repeatable true}]}]
               (sut/autocomplete "MIN(SUM(-1,1,  5)")))

      (t/is (= [{:type :FNCALL
                 :value "MIN"
                 :current-arg 1
                 :desc "Returns the minimum value in a numeric dataset."
                 :args [{:desc "The first value or range to consider when calculating the minimum value."}
                        {:desc "Additional values or ranges to consider when calculating the minimum value."
                         :opt true
                         :repeatable true}]}]
               (sut/autocomplete "MIN(1, SUM(-1,1,  5)")))

      (t/is (= [{:type :FNCALL
                 :value "MIN"
                 :current-arg 1
                 :desc "Returns the minimum value in a numeric dataset."
                 :args [{:desc "The first value or range to consider when calculating the minimum value."}
                        {:desc "Additional values or ranges to consider when calculating the minimum value."
                         :opt true
                         :repeatable true}]}]
               (sut/autocomplete "MAX(MIN(1, SUM(-1,1,  5)")))

      (t/is (= [{:type :FNCALL
                 :value "MIN"
                 :current-arg 1
                 :desc "Returns the minimum value in a numeric dataset."
                 :args [{:desc "The first value or range to consider when calculating the minimum value."}
                        {:desc "Additional values or ranges to consider when calculating the minimum value."
                         :opt true
                         :repeatable true}]}]
               (sut/autocomplete "MAX(MIN(1, SUM(-1,1,  5)")))

      (t/is (= [{:type :FNCALL
                 :value "MIN"
                 :current-arg 1
                 :desc "Returns the minimum value in a numeric dataset."
                 :args [{:desc "The first value or range to consider when calculating the minimum value."}
                        {:desc "Additional values or ranges to consider when calculating the minimum value."
                         :opt true
                         :repeatable true}]}]
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
