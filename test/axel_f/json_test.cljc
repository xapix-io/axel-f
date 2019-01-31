(ns axel-f.json-test
  #?@
   (:clj
    [(:require
      [axel-f.core :as af]
      [axel-f.functions.json :as sut]
      [clojure.test :as t])]
    :cljs
    [(:require
      [axel-f.core :as af]
      [axel-f.functions.json :as sut]
      [cljs.test :as t :include-macros true])]))

(t/deftest json
  (t/testing "Encode object"
    (t/is (= "{\"foo\":1}" (sut/json-encode {:foo 1}))))

  (t/testing "Decode string"
    (t/is (= {"foo" 1} (sut/json-decode "{\"foo\":1}")))))

(t/deftest JSONENCODE
  (let [f (af/compile "JSON.ENCODE(_)")]
    (t/is (= "{\"foo\":1}" (f {:foo 1})))))

(t/deftest JSONDECODE
  (let [f (af/compile "JSON.DECODE('{\"foo\":1}')")]
    (t/is (= {"foo" 1} (f)))))
