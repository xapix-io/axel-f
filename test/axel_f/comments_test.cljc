(ns axel-f.comments-test
  (:require [axel-f.excel :as af]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true]))
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(t/deftest comments
  (t/is (= 1
           ((af/compile ";; Line comment
                      1"))))

  (t/is (= 1
           ((af/compile "1 ;; Line comment"))))

  (t/is (= 1
           ((af/compile "1
                      ;; Line comment"))))

  (t/is (= 1
           ((af/compile ";~ Block comment ~; 1")))))
