(ns axel-f.trim-whitespaces-test
  (:require [axel-f.excel :as af]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest trim-whitespaces

  (t/is (= 1
           ((af/compile "  1  ")))))
