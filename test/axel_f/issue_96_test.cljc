(ns axel-f.issue-96-test
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [axel-f.excel :as af]))

(t/deftest coercion-to-string-in-text-functions

  (t/testing "ratio"

    (t/is (= "310.3333333333333333" ((af/compile "CONCATENATE(.x, .y, .y / .x)") {"x" 3 "y" 1})))))
