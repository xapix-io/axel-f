(ns axel-f.reader-test
  (:require [axel-f.reader :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true]))
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(t/deftest reader

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Unknown element .*"
         (sut/newline? [1 2 3]))))
