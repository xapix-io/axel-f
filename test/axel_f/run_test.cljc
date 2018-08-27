(ns axel-f.run-test
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [axel-f.core :as sut]
            [axel-f.lib :as lib]))

(t/deftest run-test

  (t/testing "run function"

    (t/testing "can execute formula as a string"

      (t/testing "with context"

        (t/is (= 0 (sut/run "foo.bar" {:foo {:bar 0}}))))

      (t/testing "without context"

        (t/is (= 0 (sut/run "0")))
        (t/is (= nil (sut/run "foo.bar")))))

    (t/testing "can execute compiled formula"

      (t/testing "with context"

        (t/is (= 0 (sut/run [:OBJREF "foo" "bar"] {:foo {:bar 0}})))
        (t/is (= 0 (sut/run ["OBJREF" "foo" "bar"] {:foo {:bar 0}}))))

      (t/testing "without context"

        (t/is (= 0 (sut/run [0])))
        (t/is (= nil (sut/run [:OBJREF "foo" "bar"])))
        (t/is (= nil (sut/run ["OBJREF" "foo" "bar"])))))

    (t/testing "bunch of corner cases"

      (t/are [x y] (t/is (= x (sut/run y)))
        1 "+1"
        1 "--TRUE"
        0 "--FALSE"
        -1 "-TRUE"
        0 "-(1<0)"
        1 "+(1<2)"
        0 "-FALSE"
        2 "1 + 1"
        0 "1 - 1"
        "ab" "\"a\" & \"b\""
        #?(:clj 1/2
           :cljs 0.5) "1 / 2"
        4 "2 * 2"
        true "1 <> 2"
        false "1 <> 1"
        true "1 = 1"
        false "1 = 2"
        true "1 <= 2"
        true " 1 <= 1"
        false "2 <= 1"
        true "1 < 2"
        false "1 < 1"
        false "1 < 0"
        true "1 >= 0"
        true "1 >= 1"
        false "1 >= 2"
        true "1 > 0"
        false "1 > 1"
        false "1 > 2"
        true "TRUE"
        true "True"
        true "true"
        false "FALSE"
        false "False"
        false "false"
        8 "(1+1)^3"
        8 "(1+1)^SUM(1,2)")

      (t/is (lib/fuzzy= 0.0001 10.0451 (sut/run "SUM(1,2)^2.1")))
      (t/is (lib/fuzzy= 0.0001 9.261 (sut/run "2.1^SUM(1,2)")))

      (t/is (thrown? #?(:clj Exception
                        :cljs js/Error) (sut/run "-\"foo\"")))
      (let [res (sut/run "ROUND()")]
        (t/is (= {:type "#N/A"
                  :reason "Wrong number of arguments to ROUND. Expected between 1 and 2 arguments, but got 0 arguments."}
                 res))))))
