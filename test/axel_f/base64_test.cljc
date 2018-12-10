(ns axel-f.base64-test
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [axel-f.core :as af]
            [axel-f.base64 :as sut]))

(t/deftest base64
  (t/testing "Encode string"
    (t/is (= "cXdl" (sut/base64-encode "qwe"))))

  (t/testing "Decode string"
    (t/is (= "qwe" (sut/base64-decode "cXdl")))))

(t/deftest BASE64ENCODE
  (t/is (= "cXdl" (af/run "BASE64.ENCODE('qwe')"))))

(t/deftest BASE64DECODE
  (t/is (= "qwe" (af/run "BASE64.DECODE('cXdl')"))))
