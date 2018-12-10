(ns axel-f.json-test
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [axel-f.core :as af]
            [axel-f.json :as sut]))

(t/deftest json
  (t/testing "Encode object"
    (t/is (= "{\"foo\":1}" (sut/json-encode {:foo 1}))))

  (t/testing "Decode string"
    (t/is (= {"foo" 1} (sut/json-decode "{\"foo\":1}")))))

(t/deftest JSONENCODE
  (t/is (= "{\"foo\":1}" (af/run "JSON.ENCODE(_)" {:foo 1}))))

(t/deftest JSONDECODE
  (t/is (= {"foo" 1} (af/run "JSON.DECODE('{\"foo\":1}')"))))
