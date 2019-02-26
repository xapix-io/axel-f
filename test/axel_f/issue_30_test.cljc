(ns axel-f.issue-30-test
  (:require  #?(:clj [clojure.test :as t]
                :cljs [cljs.test :as t :include-macros true])
             [axel-f.core :as sut]))

(t/deftest constants-precompile-and-run-test

  (t/testing "strings processed correctly"

    (t/is (= "foo" (sut/run (sut/compile "\"foo\""))))
    (t/is (= "foo" (sut/run (sut/compile "'foo'")))))

  (t/testing "numbers processed correctly"

    (t/is (= 1 (sut/run (sut/compile "1"))))
    (t/is (= 1.1 (sut/run (sut/compile "1.1"))))
    (t/is (= 1e10 (sut/run (sut/compile "1e10")))))

  (t/testing "booleans processed correctly"

    (t/is (= true (sut/run (sut/compile "true"))))
    (t/is (= false (sut/run (sut/compile "false"))))))
