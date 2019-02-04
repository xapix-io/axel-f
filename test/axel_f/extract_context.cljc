(ns axel-f.extract-context
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [axel-f.lexer :as l]
            [axel-f.parser :as p]
            [axel-f.core :as sut]))

(defn- get-context [formula]
  (-> formula
      sut/analyze
      :used-references))

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
           (get-context "SUM(foo.bar, foo.baz[*])"))))
