(ns axel-f.search-test
  (:require  #?(:clj [clojure.test :as t]
                :cljs [cljs.test :as t :include-macros true])
             [axel-f.excel :as af]
             [axel-f.excel.search :as sut]))

(t/deftest search

  (let [pattern "[\"@def-rule\", \"$find-leafs\", [\"@alt\", [\"@scan-indexed\", \"!path\", \"$find-leafs\"], \"?node\"]]"
        data {"foo" [{"bar" "qwe"} 3 4 5]}]

    (t/is (= [{"path" ["foo" 0 "bar"], "node" "qwe"}
              {"path" ["foo" 1], "node" 3}
              {"path" ["foo" 2], "node" 4}
              {"path" ["foo" 3], "node" 5}]
             (sut/json-search data pattern)
             ((af/compile (str "JSON.SEARCH(.data, '" pattern "')"))
              {"data" data})))))
