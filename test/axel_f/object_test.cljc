(ns axel-f.object-test
  (:require [axel-f.excel :as af]
            [axel-f.excel.object :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest object
  (t/is (= {"foo" 1 1 2}
           (sut/OBJECT [["foo" 1] [1 2]])))

  (t/is (= {"foo" 1 1 2}
           ((af/eval "OBJECT.NEW({{'foo', 1}, {1, 2}})")))))

(t/deftest object-merge
  (t/is (= {"foo" 1 0 2 :baz 3}
           (sut/MERGE {"foo" 1} {0 2} {:baz 3})))

  (t/is (= {"foo" 1 0 2 :baz 3}
           ((af/eval "OBJECT.MERGE({{'foo', 1}}, {{0, 2}}, _)") {:baz 3}))))
