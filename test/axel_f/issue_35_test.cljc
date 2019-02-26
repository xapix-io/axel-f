(ns axel-f.issue-35-test
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [axel-f.core :as sut]
            [clojure.string :as string])
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(t/deftest object-references-could-be-anything

  (t/testing "fields with unicode symbols inside"

    (t/are [x] (= 1 (sut/run (sut/compile x) {(keyword (string/replace x #"\\" "")) 1}))
      "ꙮ"
      "fooꙮbar"
      "foo\\-bar"))

  (t/testing "quoted fields with operators inside"

    (t/are [x] (= 1 (sut/run (sut/compile x) {(keyword (string/replace x #"\\" "")) 1}))
      "foo\\/bar"
      "foo\\+"
      "foo\\*bar"
      "foo\\>bar"
      "foo\\ \\>\\ bar"
      "foo\\^bar"
      "foo\\%bar"
      "foo\\&bar"))

  (t/testing "fields with special symbols throw an error without quoting"

    (t/are [x] (thrown? ExceptionInfo
                        (sut/run (sut/compile x)))
      "foo.abr baz"
      "foo.bar."
      "foo.bar,baz"
      "foo.bar\""
      "foo.bar'"
      "foo.bar["
      "foo.bar("
      "foo.bar]"
      "foo.bar)")))
