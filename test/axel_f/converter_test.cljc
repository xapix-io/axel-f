(ns axel-f.converter-test
  (:require  #?(:clj [clojure.test :as t]
                :cljs [cljs.test :as t :include-macros true])
             [axel-f.core :as af]
             [axel-f.functions.convert :as sut]))

(t/deftest base64
  (t/testing "Encode string"
    (t/is (= "cXdl" (sut/base64-encode "qwe"))))

  (t/testing "Decode string"
    (t/is (= "qwe" (sut/base64-decode "cXdl")))))

(t/deftest json
  (t/testing "Encode object"
    (t/is (= "{\"foo\":1}" (sut/json-encode {:foo 1}))))

  (t/testing "Decode string"
    (t/is (= {"foo" 1} (sut/json-decode "{\"foo\":1}")))))

(t/deftest BASE64ENCODE
  (t/is (= "cXdl" (af/run "BASE64ENCODE('qwe')"))))

(t/deftest BASE64DECODE
  (t/is (= "qwe" (af/run "BASE64DECODE('cXdl')"))))

(t/deftest JSONENCODE
  (t/is (= "{\"foo\":1}" (af/run "JSONENCODE(_)" {:foo 1}))))

(t/deftest JSONDECODE
  (t/is (= {"foo" 1} (af/run "JSONDECODE('{\"foo\":1}')"))))

(t/deftest complex-example
  (t/is (= 1 (af/run "JSONDECODE(BASE64DECODE(_)).foo" "eyJmb28iOjF9"))))
