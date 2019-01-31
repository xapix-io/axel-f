(ns axel-f.base64-test
  #?@
   (:clj
    [(:require
      [axel-f.core :as af]
      [axel-f.functions.base64 :as sut]
      [clojure.test :as t])]
    :cljs
    [(:require
      [axel-f.core :as af]
      [axel-f.functions.base64 :as sut]
      [cljs.test :as t :include-macros true])]))

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
