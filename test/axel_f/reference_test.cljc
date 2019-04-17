(ns axel-f.reference-test
  (:require [axel-f.excel :as af]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest list-refs
  (t/is (= '(1 2 3)
           ((af/eval "_.[*]") [1 2 3])))

  (t/is (= '(1 2 3)
           ((af/eval "[*]") [1 2 3])))

  (t/is (= 2
           ((af/eval "[1 + 0]") [1 2 3]))))
