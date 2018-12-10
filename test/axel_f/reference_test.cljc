(ns axel-f.reference-test
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [axel-f.core :as sut]))

(t/deftest reference-test

  (t/testing "references parsed into :OBJREF token"

    (t/is (= [:OBJREF "foo"]
             (sut/compile "foo")))
    (t/is (= [:OBJREF "foo" "bar"]
             (sut/compile "foo.bar")))

    (t/testing "with array references"

      (t/is (= [:OBJREF "foo" "bar" 1]
               (sut/compile "foo.bar[1]")))
      (t/is (= [:OBJREF "foo" 1 "bar"]
               (sut/compile "foo[1].bar")))
      (t/is (= [:OBJREF "foo" "*"]
               (sut/compile "foo[*]")))
      (t/is (= [:OBJREF "foo" "*"]
               (sut/compile "foo.[*]")))
      (t/is (= [:OBJREF "foo" "*" "bar"]
               (sut/compile "foo[*].bar")
               (sut/compile "foo[].bar")
               (sut/compile "foo.[].bar")))
      (t/is (= [:OBJREF "foo" "*" "bar" "*"]
               (sut/compile "foo.[*].bar[*]")
               (sut/compile "foo[].bar[]")
               (sut/compile "foo.[].bar.[]")))
      (t/is (= [:OBJREF "*" "foo"]
               (sut/compile "[].foo")
               (sut/compile "[*].foo"))))

    (t/testing "with calculated keywords"

      (t/is (= [:OBJREF "foo" [:FNCALL "CONCATENATE" [[:STRING "b"] [:STRING "a"] [:STRING "r"]]]]
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
      "foo[1].bar" {:foo [nil {:bar 1}]}
      [:OBJREF "foo" 1.2] {:foo {1.2 1}}
      [:OBJREF "foo" 1.2] {:foo {:1.2 1}})

    (t/is (= 1 (sut/run [:OBJREF :foo "bar"] {:foo {:bar 1}})))
    (t/is (= 1 (sut/run [:OBJREF :foo "bar"] {"foo" {:bar 1}}))))

  (t/testing "references evaluated into nil if corresponding path not exists in context"

    (t/are [x y] (t/is (= nil (sut/run x y)))
      "foo.bar" {"foo" {"bar1" 1}}
      "foo.bar" {"foo1" {:bar 1}}
      "foo.bar" {:foo {"bar1" 1}}
      "foo.bar" {:foo1 {:bar 1}}
      "foo.bar[1]" {:foo {:bar [1]}}
      "foo.bar[SUM(1)]" {:foo {:bar [1]}}
      "foo[1].bar" {:foo [{:bar 1}]}
      "foo.bar" nil
      "foo[1].bar" nil
      [:OBJREF "foo" 1.2] {:foo 1}))

  (t/testing "references with array wildcarts"

    (t/are [f c r] (t/is (= r (sut/run f c)))
      "foo[*]"            {:foo [1 2 3]}                               [1 2 3]
      "foo.[*]"           {:foo [1 2 3]}                               [1 2 3]
      "foo[*].bar"        {:foo [{:bar 1} {:bar 2} {:bar 3}]}          [1 2 3]
      "foo[*].bar[*].baz" {:foo [{:bar [{:baz 1} {:baz 2} {:baz 3}]}
                                 {:bar [{:baz 4} {:baz 5} {:baz 6}]}]} [[1 2 3] [4 5 6]]
      "foo[*]"            {:foo 1}                                     1
      "foo[*].bar"        {:foo []}                                    []
      "[*].foo"           [{:foo 1} {:foo 2}]                          [1 2]))

  (t/testing "dynamic references"

    (t/is (= 1 (sut/run "OBJREF({1,2,3}, 0)")))

    (t/is (= [{:bar 1} {:bar 2} {:bar 3}]
             (sut/run "FILTER(foo, _.bar < 4)" {:foo [{:bar 1} {:bar 4} {:bar 2} {:bar 3}]}))))

  (t/testing "dynamic references evaluates into full context object"

    (t/is (= 2 (sut/run "1 + _" 1)))

    (t/is (= {:foo 1 :bar 2} (sut/run "IF(foo = 1, _, #VALUE!)" {:foo 1 :bar 2})))

    (t/is (= [{:bar 1} {:bar 2} {:bar 3}]
             (sut/run "FILTER(_.foo, _.bar < 4)" {:foo [{:bar 1} {:bar 4} {:bar 2} {:bar 3}]}))))

  (t/testing "quoted references with restricted symbols"

    (t/is (= 1 (sut/run "foo.#'bar > 1'[0]" {:foo {"bar > 1" [1 2 3]}})))))

(t/deftest qualified-keywords-in-objref

  (t/is (= 1 (sut/run ":axel-f.reference-test/test" {::test 1})))

  (t/testing "mixing with dynamic references"

    (t/is (= 1 (sut/run "_.:axel-f.reference-test/test" {::test 1})))
    (t/is (= [{::b 1} {::b 2}]
             (sut/run "FILTER(:axel-f.reference-test/to-filter, _.:axel-f.reference-test/b < 4)"
               {::to-filter [{::b 1} {::b 2} {::b 5}]})))))
