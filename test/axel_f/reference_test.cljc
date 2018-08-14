(ns axel-f.reference-test
  (:require [clojure.test :as t]
            [axel-f.core :as sut]))

(t/deftest reference-test

  (t/testing "references parsed into :OBJREF token"

    (t/is (= [:OBJREF "foo"]
             (sut/compile "foo")))
    (t/is (= [:OBJREF "foo" "bar"]
             (sut/compile "foo.bar")))
    (t/is (= [:OBJREF "foo" "bar" 1]
             (sut/compile "foo.bar[1]")))
    (t/is (= [:OBJREF "foo" 1 "bar"]
             (sut/compile "foo[1].bar")))

    (t/testing "with calculated keywords"

      (t/is (= [:OBJREF "foo" [:FNCALL "CONCATENATE" ["b" "a" "r"]]]
               (sut/compile "foo.CONCATENATE(\"b\", \"a\", \"r\")")))
      (t/is (= [:OBJREF "foo" [:FNCALL "SUM" [1 2]]]
               (sut/compile "foo.SUM(1,2)")))
      (t/is (= [:OBJREF "foo" [:FNCALL "SUM" [0]]]
               (sut/compile "foo[SUM(0)]")))))

  (t/testing "references evaluated into the value if corresponding path exists in context"

    (t/are [x y] (t/is (= 1 (sut/run x y)))
      "foo.bar" {"foo" {"bar" 1}}
      "foo.bar" {"foo" {:bar 1}}
      "foo.bar" {:foo {"bar" 1}}
      "foo.bar" {:foo {:bar 1}}
      "foo.bar[0]" {:foo {:bar [1]}}
      "foo.bar[SUM(0)]" {:foo {:bar [1]}}
      "foo[1].bar" {:foo [nil {:bar 1}]}))

  (t/testing "references evaluated into nil if corresponding path not exists in context"

    (t/are [x y] (t/is (= nil (sut/run x y)))
      "foo.bar" {"foo" {"bar1" 1}}
      "foo.bar" {"foo1" {:bar 1}}
      "foo.bar" {:foo {"bar1" 1}}
      "foo.bar" {:foo1 {:bar 1}}
      "foo.bar[1]" {:foo {:bar [1]}}
      "foo.bar[SUM(1)]" {:foo {:bar [1]}}
      "foo[1].bar" {:foo [{:bar 1}]})))
