(ns axel-f.converter-test
  (:require  #?(:clj [clojure.test :as t]
                :cljs [cljs.test :as t :include-macros true])
             [axel-f.core :as af]
             axel-f.json
             axel-f.base64))

(t/deftest complex-example
  (t/is (= 1 (af/run "JSON.DECODE(BASE64.DECODE(_)).foo" "eyJmb28iOjF9"))))
