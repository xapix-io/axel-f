(ns axel-f.core-test
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [axel-f.excel :as af]))

(t/deftest empty-formula
  (t/is (nil? ((af/compile ""))))
  (t/is (nil? ((af/compile "") {}))))
