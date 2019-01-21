(ns axel-f.reference-test
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [axel-f.core :as sut]))

(t/deftest reference-test

  (t/testing "references parsed into :OBJREF token"

    (t/is (= {:kind :axel-f.v2.parser/fncall,
              :f :axel-f.v2.parser/reference,
              :args
              [{:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/symbol,
                :arg "foo",
                :begin {:line 1, :column 1},
                :end {:line 1, :column 3}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 3}}
             (sut/compile "foo")))
    (t/is (= {:kind :axel-f.v2.parser/fncall,
              :f :axel-f.v2.parser/reference,
              :args
              [{:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/symbol,
                :arg "foo",
                :begin {:line 1, :column 1},
                :end {:line 1, :column 3}}
               {:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/reference,
                :args
                [{:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/symbol,
                  :arg "bar",
                  :begin {:line 1, :column 5},
                  :end {:line 1, :column 7}}],
                :begin {:line 1, :column 5},
                :end {:line 1, :column 7}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 3}}
             (sut/compile "foo.bar")))

    (t/testing "with array references"

      (t/is (= {:kind :axel-f.v2.parser/fncall,
           :f :axel-f.v2.parser/reference,
           :args
           [{:kind :axel-f.v2.parser/fncall,
             :f :axel-f.v2.parser/symbol,
             :arg "foo",
             :begin {:line 1, :column 1},
             :end {:line 1, :column 3}}
            {:kind :axel-f.v2.parser/fncall,
             :f :axel-f.v2.parser/reference,
             :args
             [{:kind :axel-f.v2.parser/fncall,
               :f :axel-f.v2.parser/symbol,
               :arg "bar",
               :begin {:line 1, :column 5},
               :end {:line 1, :column 7}}],
             :begin {:line 1, :column 5},
             :end {:line 1, :column 7}}
            {:kind :axel-f.v2.parser/fncall,
             :f :axel-f.v2.parser/nth,
             :arg
             {:kind :axel-f.v2.parser/fncall,
              :f :axel-f.v2.parser/const,
              :arg 1,
              :begin {:line 1, :column 9},
              :end {:line 1, :column 9}},
             :begin {:line 1, :column 8},
             :end {:line 1, :column 10}}],
           :begin {:line 1, :column 1},
           :end {:line 1, :column 3}}
               (sut/compile "foo.bar[1]")))
      (t/is (= {:kind :axel-f.v2.parser/fncall,
           :f :axel-f.v2.parser/reference,
           :args
           [{:kind :axel-f.v2.parser/fncall,
             :f :axel-f.v2.parser/symbol,
             :arg "foo",
             :begin {:line 1, :column 1},
             :end {:line 1, :column 3}}
            {:kind :axel-f.v2.parser/fncall,
             :f :axel-f.v2.parser/nth,
             :arg
             {:kind :axel-f.v2.parser/fncall,
              :f :axel-f.v2.parser/const,
              :arg 1,
              :begin {:line 1, :column 5},
              :end {:line 1, :column 5}},
             :begin {:line 1, :column 4},
             :end {:line 1, :column 6}}
            {:kind :axel-f.v2.parser/fncall,
             :f :axel-f.v2.parser/reference,
             :args
             [{:kind :axel-f.v2.parser/fncall,
               :f :axel-f.v2.parser/symbol,
               :arg "bar",
               :begin {:line 1, :column 8},
               :end {:line 1, :column 10}}],
             :begin {:line 1, :column 8},
             :end {:line 1, :column 10}}],
           :begin {:line 1, :column 1},
           :end {:line 1, :column 3}}
               (sut/compile "foo[1].bar")))
      (t/is (= {:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/reference,
                :args
                [{:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/symbol,
                  :arg "foo",
                  :begin {:line 1, :column 1},
                  :end {:line 1, :column 3}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/nth,
                  :arg
                  {:kind :axel-f.v2.parser/fncall,
                   :f :axel-f.v2.parser/ALL,
                   :arg nil},
                  :begin {:line 1, :column 4},
                  :end {:line 1, :column 6}}],
                :begin {:line 1, :column 1},
                :end {:line 1, :column 3}}
               (sut/compile "foo[*]")))
      (t/is (= {:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/reference,
                :args
                [{:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/symbol,
                  :arg "foo",
                  :begin {:line 1, :column 1},
                  :end {:line 1, :column 3}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/nth,
                  :arg
                  {:kind :axel-f.v2.parser/fncall,
                   :f :axel-f.v2.parser/ALL,
                   :arg nil},
                  :begin {:line 1, :column 5},
                  :end {:line 1, :column 7}}],
                :begin {:line 1, :column 1},
                :end {:line 1, :column 3}}
               (sut/compile "foo.[*]")))
      (t/is (= {:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/reference,
                :args
                [{:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/symbol,
                  :arg "foo",
                  :begin {:line 1, :column 1},
                  :end {:line 1, :column 3}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/nth,
                  :arg
                  {:kind :axel-f.v2.parser/fncall,
                   :f :axel-f.v2.parser/ALL,
                   :arg nil},
                  :begin {:line 1, :column 4},
                  :end {:line 1, :column 6}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/reference,
                  :args
                  [{:kind :axel-f.v2.parser/fncall,
                    :f :axel-f.v2.parser/symbol,
                    :arg "bar",
                    :begin {:line 1, :column 8},
                    :end {:line 1, :column 10}}],
                  :begin {:line 1, :column 8},
                  :end {:line 1, :column 10}}],
                :begin {:line 1, :column 1},
                :end {:line 1, :column 3}}
               (sut/compile "foo[*].bar")
               ))
      (t/is (= {:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/reference,
                :args
                [{:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/symbol,
                  :arg "foo",
                  :begin {:line 1, :column 1},
                  :end {:line 1, :column 3}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/nth,
                  :arg
                  {:kind :axel-f.v2.parser/fncall,
                   :f :axel-f.v2.parser/ALL,
                   :arg nil},
                  :begin {:line 1, :column 4},
                  :end {:line 1, :column 5}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/reference,
                  :args
                  [{:kind :axel-f.v2.parser/fncall,
                    :f :axel-f.v2.parser/symbol,
                    :arg "bar",
                    :begin {:line 1, :column 7},
                    :end {:line 1, :column 9}}],
                  :begin {:line 1, :column 7},
                  :end {:line 1, :column 9}}],
                :begin {:line 1, :column 1},
                :end {:line 1, :column 3}}
               (sut/compile "foo[].bar")))
      (t/is (= {:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/reference,
                :args
                [{:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/symbol,
                  :arg "foo",
                  :begin {:line 1, :column 1},
                  :end {:line 1, :column 3}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/nth,
                  :arg
                  {:kind :axel-f.v2.parser/fncall,
                   :f :axel-f.v2.parser/ALL,
                   :arg nil},
                  :begin {:line 1, :column 5},
                  :end {:line 1, :column 6}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/reference,
                  :args
                  [{:kind :axel-f.v2.parser/fncall,
                    :f :axel-f.v2.parser/symbol,
                    :arg "bar",
                    :begin {:line 1, :column 8},
                    :end {:line 1, :column 10}}],
                  :begin {:line 1, :column 8},
                  :end {:line 1, :column 10}}],
                :begin {:line 1, :column 1},
                :end {:line 1, :column 3}}
               (sut/compile "foo.[].bar")))
      (t/is (= {:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/reference,
                :args
                [{:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/symbol,
                  :arg "foo",
                  :begin {:line 1, :column 1},
                  :end {:line 1, :column 3}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/nth,
                  :arg
                  {:kind :axel-f.v2.parser/fncall,
                   :f :axel-f.v2.parser/ALL,
                   :arg nil},
                  :begin {:line 1, :column 5},
                  :end {:line 1, :column 7}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/reference,
                  :args
                  [{:kind :axel-f.v2.parser/fncall,
                    :f :axel-f.v2.parser/symbol,
                    :arg "bar",
                    :begin {:line 1, :column 9},
                    :end {:line 1, :column 11}}],
                  :begin {:line 1, :column 9},
                  :end {:line 1, :column 11}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/nth,
                  :arg
                  {:kind :axel-f.v2.parser/fncall,
                   :f :axel-f.v2.parser/ALL,
                   :arg nil},
                  :begin {:line 1, :column 12},
                  :end {:line 1, :column 14}}],
                :begin {:line 1, :column 1},
                :end {:line 1, :column 3}}
               (sut/compile "foo.[*].bar[*]")))
      #_(t/is (= [:OBJREF "*" "foo"]
               (sut/compile "[].foo")
               (sut/compile "[*].foo"))))

    (t/testing "with calculated keywords"

      (t/is (= {:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/reference,
                :args
                [{:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/symbol,
                  :arg "foo",
                  :begin {:line 1, :column 1},
                  :end {:line 1, :column 3}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f "CONCATENATE",
                  :args
                  [{:kind :axel-f.v2.parser/fncall,
                    :f :axel-f.v2.parser/const,
                    :arg "b",
                    :begin {:line 1, :column 18},
                    :end {:line 1, :column 20}}
                   {:kind :axel-f.v2.parser/fncall,
                    :f :axel-f.v2.parser/const,
                    :arg "a",
                    :begin {:line 1, :column 23},
                    :end {:line 1, :column 25}}
                   {:kind :axel-f.v2.parser/fncall,
                    :f :axel-f.v2.parser/const,
                    :arg "r",
                    :begin {:line 1, :column 28},
                    :end {:line 1, :column 30}}]}],
                :begin {:line 1, :column 1},
                :end {:line 1, :column 3}}
               (sut/compile "foo.(CONCATENATE(\"b\", \"a\", \"r\"))")))
      (t/is (= {:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/reference,
                :args
                [{:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/symbol,
                  :arg "foo",
                  :begin {:line 1, :column 1},
                  :end {:line 1, :column 3}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/nth,
                  :arg
                  {:kind :axel-f.v2.parser/fncall,
                   :f "SUM",
                   :args
                   [{:kind :axel-f.v2.parser/fncall,
                     :f :axel-f.v2.parser/const,
                     :arg 0,
                     :begin {:line 1, :column 9},
                     :end {:line 1, :column 9}}]},
                  :begin {:line 1, :column 4},
                  :end {:line 1, :column 11}}],
                :begin {:line 1, :column 1},
                :end {:line 1, :column 3}}
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
      "foo[1].bar" {:foo [{:bar 1}]}
      "foo.bar" nil
      "foo[1].bar" nil))

  (t/testing "references with array wildcarts"

    (t/are [f c r] (t/is (= r (sut/run f c)))
      "foo[*]"            {:foo [1 2 3]}                               [1 2 3]
      "foo.[*]"           {:foo [1 2 3]}                               [1 2 3]
      "foo[*].bar"        {:foo [{:bar 1} {:bar 2} {:bar 3}]}          [1 2 3]
      "foo[*].bar[*].baz" {:foo [{:bar [{:baz 1} {:baz 2} {:baz 3}]}
                                 {:bar [{:baz 4} {:baz 5} {:baz 6}]}]} [[1 2 3] [4 5 6]]
      "foo[*].bar"        {:foo []}                                    []
      "_.[*].foo"         [{:foo 1} {:foo 2}]                          [1 2]))

  (t/testing "dynamic references"

    #_(t/is (= 1 (sut/run "OBJREF({1,2,3}, 0)")))

    (t/is (= [{:bar 1} {:bar 2} {:bar 3}]
             (sut/run "FILTER(_.bar < 4, foo)" {:foo [{:bar 1} {:bar 4} {:bar 2} {:bar 3}]}))))

  (t/testing "dynamic references evaluates into full context object"

    (t/is (= 2 (sut/run "1 + _" 1)))

    (t/is (= {:foo 1 :bar 2} (sut/run "IF(foo = 1, _, \"!VALUE\")" {:foo 1 :bar 2})))

    (t/is (= [{:bar 1} {:bar 2} {:bar 3}]
             (sut/run "FILTER(_.bar < 4, _.foo)" {:foo [{:bar 1} {:bar 4} {:bar 2} {:bar 3}]}))))

  (t/testing "quoted references with restricted symbols"

    (t/is (= 1 (sut/run "foo.'bar > 1'[0]" {:foo {"bar > 1" [1 2 3]}})))))

(t/deftest qualified-keywords-in-objref

  (t/is (= 1 (sut/run ":axel\\-f.reference\\-test/test" {::test 1})))

  (t/is (= {:kind :axel-f.v2.parser/fncall,
            :f :axel-f.v2.parser/reference,
            :args
            [{:kind :axel-f.v2.parser/fncall,
              :f :axel-f.v2.parser/keyword,
              :arg :axel-f.reference-test/test,
              :begin {:line 1, :column 1},
              :end {:line 1, :column 29}}],
            :begin {:line 1, :column 1},
            :end {:line 1, :column 29}}
           (sut/compile ":axel\\-f.reference\\-test/test")))

  (t/testing "mixing with dynamic references"

    (t/is (= 1 (sut/run "_.:axel\\-f.reference\\-test/test" {::test 1})))
    (t/is (= [{::b 1} {::b 2}]
             (sut/run "FILTER(_.:axel\\-f.reference\\-test/b < 4, :axel\\-f.reference\\-test/to\\-filter)"
               {::to-filter [{::b 1} {::b 2} {::b 5}]})))))
