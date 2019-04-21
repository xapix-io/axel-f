(ns axel-f.issue-63-test
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
             [axel-f.excel :as af]))

(t/deftest issue-63

  (t/is (= 0.3 ((af/compile "MAX(0.0, (0.6 * (1.0 - 0.5)))")))))
