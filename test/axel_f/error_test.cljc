(ns axel-f.error-test
  (:require [axel-f.error :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest error-test

  (t/testing "error function returns error instance with additional information"

    (t/is (instance? #?(:clj clojure.lang.ExceptionInfo
                        :cljs js/Error)
                     (sut/error "FOO")))

    (t/is (= "FOO" (#?(:clj .getMessage
                       :cljs .-message)
                    (sut/error "FOO"))))

    (t/is (= {:type "FOO"} (#?(:clj ex-data
                               :cljs .-data)
                            (sut/error "FOO"))))

    (t/is (= {:reason {:foo "1"}
              :type "FOO"} (#?(:clj ex-data
                            :cljs .-data)
                         (sut/error "FOO" {:foo "1"}))))))
