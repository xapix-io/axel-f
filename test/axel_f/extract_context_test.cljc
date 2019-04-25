(ns axel-f.extract-context-test
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [axel-f.excel :as sut]))

(defn- get-context [formula]
  (-> formula
      sut/compile
      meta
      :free-variables))

(t/deftest derive-context

  (t/is (= [["foo" "bar" "baz"]]
           (get-context "foo.bar.baz")))

  (t/is (= [["foo" "*" "baz"]]
           (get-context "foo[*].baz")
           (get-context "foo.[*].baz")
           (get-context "foo[2].baz")))

  (t/is (= [["*"]]
           (get-context "[*]")))

  (t/is (= [["*" "bar"]]
           (get-context "[*].bar")))

  (t/is (= [["*" "bar" "*"]]
           (get-context "[*].bar[*]")))

  (t/is (= [["foo" "bar"] ["foo" "baz"]]
           (get-context "foo.bar + foo.baz")))

  (t/is (= [["foo" "bar" "*" "baz"] ["foo" "booz"]]
           (get-context "foo.bar[foo.booz].baz")))

  (t/is (= [["foo" "bar"]]
           (get-context "!foo.bar")))

  (t/is (= [["foo" "bar"] ["foo" "baz"]]
           (get-context "{1, foo.bar, foo.baz}")))

  (t/is (= [["foo" "bar"] ["foo" "baz" "*"]]
           (get-context "SUM(foo.bar, foo.baz[*])")))

  (t/is (= [["foo" "bar"]]
           (get-context "SUM(foo.bar, foo.bar)")))

  (t/is (= [["var" "in" "group"]]
           (get-context "2 * (2 + var.in.group)")))

  (t/is (= [["foo" "bar"] ["foo" "baz"]]
           (get-context "MAP(FN(x, x + foo.bar), foo.baz)")))

  (t/is (= [["foo" "bar" "*"] ["foo" "baz"]]
           (get-context "WITH(x, 1, y, foo.bar[2] + x, SUM(foo.baz, x, y))"))))
