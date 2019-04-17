(ns axel-f.json-test
   (:require #?(:clj [clojure.test :as t]
                :cljs [cljs.test :as t :include-macros true])
             [axel-f.excel :as af]
             [axel-f.excel.json :as sut]))

(t/deftest json
  (t/testing "Encode object"
    (t/is (= "{\"foo\":1}" (sut/json-encode {:foo 1}))))

  (t/testing "Decode string"
    (t/is (= {"foo" 1} (sut/json-decode "{\"foo\":1}")))))

(t/deftest JSONENCODE
  (let [f (af/eval "JSON.ENCODE(_)")]
    (t/is (= "{\"foo\":1}" (f {:foo 1})))))

(t/deftest JSONDECODE
  (let [f (af/eval "JSON.DECODE('{\"foo\":1}')")]
    (t/is (= {"foo" 1} (f)))))
