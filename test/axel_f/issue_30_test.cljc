(ns axel-f.issue-30-test
  (:require  #?(:clj [clojure.test :as t]
                :cljs [cljs.test :as t :include-macros true])
             [axel-f.excel :as af]))

(t/deftest constants-precompile-and-run-test

  (t/testing "strings processed correctly"

    (t/is (= "foo" ((af/eval "\"foo\""))))
    (t/is (= "foo" ((af/eval "'foo'")))))

  (t/testing "numbers processed correctly"

    (t/is (= 1 ((af/eval "1"))))
    (t/is (= 1.1 ((af/eval "1.1"))))
    (t/is (= 1e10 ((af/eval "1e10"))))
    (t/is (= 1e-10 ((af/eval "1E-10")))))

  (t/testing "booleans processed correctly"

    (t/is (= true ((af/eval "true"))))
    (t/is (= false ((af/eval "false"))))))
