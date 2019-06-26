(ns axel-f.base64-test
   (:require [axel-f.excel :as af]
             [axel-f.excel.base64 :as sut]
             #?(:clj [clojure.test :as t]
                :cljs [cljs.test :as t :include-macros true])))

(t/deftest base64
  (t/testing "Encode string"
    (t/is (= "cXdl" (sut/base64-encode "qwe"))))

  (t/testing "Decode string"
    (t/is (= "qwe" (sut/base64-decode "cXdl")))))

(t/deftest BASE64ENCODE
  (let [f (af/compile "BASE64.ENCODE('qwe')")]
    (t/is (= "cXdl" (f)))))

(t/deftest BASE64DECODE
  (let [f (af/compile "BASE64.DECODE('cXdl')")]
    (t/is (= "qwe" (f)))))
