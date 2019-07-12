(ns axel-f.issue-79-test
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [axel-f.excel :as af]))

(t/deftest arguments-for-binary-expression

  (t/testing "arguments for binary expression"

    (t/testing "can be prefix expression"

      (t/is (= 0 ((af/compile "1 + -1"))))
      (t/is (= 0 ((af/compile "-1 + 1")))))

    (t/testing "can be postfix expression"

      (t/is (= 1.01 ((af/compile "1 + 1%"))))

      (t/is (= 1.01 ((af/compile "1% + 1"))))))

  (t/testing "arguments for prefix expression"

    (t/testing "can be another prefix expression"

      (t/is (= 1 ((af/compile "--1")))))

    (t/testing "can be postfix expression"

      (t/is (= -0.1 ((af/compile "-10%"))))))

  (t/testing "examples from report"

    (t/is (= 0 ((af/compile "1 + -1") {})))
    (t/is (= -1 ((af/compile "1 * -1") {})))
    (t/is (= -1 ((af/compile "1 / -1") {})))
    (t/is (= 1 ((af/compile "-1 + 2") {})))
    (t/is (= 0 ((af/compile "-1 + 1") {})))))
