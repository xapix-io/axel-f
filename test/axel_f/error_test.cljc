(ns axel-f.error-test
  (:require [axel-f.error :as sut]
            [axel-f.core :as af-sut]
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
                            (sut/error "FOO" {:foo "1"})))))

  (t/testing "errors returned bu the formula passed from run function"

    (t/is (= {:type "#VALUE!"}
             (#?(:clj ex-data
                 :cljs .-data)
              (af-sut/run "#VALUE!")))))

  #?(:clj
     (t/testing "non-excel errors are thrown from run function"

       (t/is (thrown? java.lang.ArithmeticException
                      (af-sut/run "1 / 0"))))))
