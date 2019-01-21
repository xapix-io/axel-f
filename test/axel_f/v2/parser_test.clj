(ns axel-f.v2.parser-test
  (:require [axel-f.v2.parser :as sut]
            [clojure.test :as t]))

(t/deftest parse-tests

  (t/testing "parsing keywords"

    (t/is (= {:kind :axel-f.v2.parser/fncall,
              :f :axel-f.v2.parser/reference,
              :args
              [{:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/keyword,
                :arg :foo.bar/baz,
                :begin {:line 1, :column 1},
                :end {:line 1, :column 12}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 12}}
             (sut/parse ":foo.bar/baz"))))

  (t/testing "parsing constants"

    (t/testing "as a number"

      (t/is (= {:kind :axel-f.v2.parser/fncall
                :f :axel-f.v2.parser/const
                :arg 1
                :begin {:line 1 :column 1}
                :end {:line 1 :column 1}}
               (sut/parse "1"))))

    (t/testing "as a string"

      (t/is (= {:kind :axel-f.v2.parser/fncall
                :f :axel-f.v2.parser/const
                :arg "string"
                :begin {:line 1 :column 1}
                :end {:line 1 :column 8}}
               (sut/parse "'string'")))

      (t/is (= {:kind :axel-f.v2.parser/fncall
                :f :axel-f.v2.parser/const
                :arg "string"
                :begin {:line 1 :column 1}
                :end {:line 1 :column 8}}
               (sut/parse "\"string\"")))))

  (t/testing "parsing vector of constants"

    (t/is (= {:kind :axel-f.v2.parser/fncall,
              :f :axel-f.v2.parser/vector,
              :args
              [{:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/const,
                :arg 1,
                :begin {:line 1, :column 2},
                :end {:line 1, :column 2}}
               {:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/const,
                :arg 2,
                :begin {:line 1, :column 5},
                :end {:line 1, :column 5}}]}
             (sut/parse "{1, 2}")))

    (t/is (= {:kind :axel-f.v2.parser/fncall,
              :f :axel-f.v2.parser/vector,
              :args
              [{:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/const,
                :arg 1,
                :begin {:line 1, :column 2},
                :end {:line 1, :column 2}}
               {:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/const,
                :arg "foo",
                :begin {:line 1, :column 5},
                :end {:line 1, :column 9}}]}
             (sut/parse "{1, \"foo\"}"))))

  (t/testing "parsing simple arythmetic expressions"

    (t/is (= {:kind :axel-f.v2.parser/fncall,
              :f "-",
              :args
              [{:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/const,
                :arg 1,
                :begin {:line 1, :column 2},
                :end {:line 1, :column 2}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 2}}
             (sut/parse "-1")))

    (t/is (= {:kind :axel-f.v2.parser/fncall,
              :f "+",
              :args
              [{:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/const,
                :arg 1,
                :begin {:line 1, :column 1},
                :end {:line 1, :column 1}}
               {:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/const,
                :arg 1,
                :begin {:line 1, :column 5},
                :end {:line 1, :column 5}}]}
             (sut/parse "1 + 1")))

    (t/is (= {:kind :axel-f.v2.parser/fncall,
              :f "!",
              :args
              [{:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/const,
                :arg 1,
                :begin {:line 1, :column 2},
                :end {:line 1, :column 2}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 2}}
             (sut/parse "!1")))

    (t/is (= {:kind :axel-f.v2.parser/fncall,
              :f "%",
              :args
              [{:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/const,
                :arg 10,
                :begin {:line 1, :column 1},
                :end {:line 1, :column 2}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 3}}
             (sut/parse "10%"))))

  (t/testing "parsing reference expression"

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
                :f :axel-f.v2.parser/reference,
                :args
                [{:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/symbol,
                  :arg "baz",
                  :begin {:line 1, :column 9},
                  :end {:line 1, :column 11}}],
                :begin {:line 1, :column 9},
                :end {:line 1, :column 11}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 3}}
             (sut/parse "foo.bar.baz")))

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
                :end {:line 1, :column 6}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 3}}
             (sut/parse "foo[1]")))

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
                 :f "+",
                 :args
                 [{:kind :axel-f.v2.parser/fncall,
                   :f :axel-f.v2.parser/const,
                   :arg 1,
                   :begin {:line 1, :column 6},
                   :end {:line 1, :column 6}}
                  {:kind :axel-f.v2.parser/fncall,
                   :f :axel-f.v2.parser/const,
                   :arg 1,
                   :begin {:line 1, :column 8},
                   :end {:line 1, :column 8}}]},
                :begin {:line 1, :column 5},
                :end {:line 1, :column 9}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 3}}
             (sut/parse "foo.[1+1]")))

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
                 :f :axel-f.v2.parser/ALL,
                 :arg nil},
                :begin {:line 1, :column 8},
                :end {:line 1, :column 10}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 3}}
             (sut/parse "foo.bar[*]")))

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
                 :f :axel-f.v2.parser/ALL,
                 :arg nil},
                :begin {:line 1, :column 9},
                :end {:line 1, :column 11}}
               {:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/reference,
                :args
                [{:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/symbol,
                  :arg "baz",
                  :begin {:line 1, :column 13},
                  :end {:line 1, :column 15}}],
                :begin {:line 1, :column 13},
                :end {:line 1, :column 15}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 3}}
             (sut/parse "foo.bar.[*].baz")))

    (t/is (= {:kind :axel-f.v2.parser/fncall,
              :f "MAP"
              :args
              [{:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/reference,
                :args
                [{:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/symbol,
                  :arg "_",
                  :begin {:line 1, :column 5},
                  :end {:line 1, :column 5}}],
                :begin {:line 1, :column 5},
                :end {:line 1, :column 5}}
               {:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/vector,
                :args
                [{:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/const,
                  :arg 1,
                  :begin {:line 1, :column 9},
                  :end {:line 1, :column 9}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/const,
                  :arg 2,
                  :begin {:line 1, :column 12},
                  :end {:line 1, :column 12}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/const,
                  :arg 3,
                  :begin {:line 1, :column 15},
                  :end {:line 1, :column 15}}]}]}
             (sut/parse "MAP(_, {1, 2, 3})")))

    (t/is (= {:kind :axel-f.v2.parser/fncall,
              :f "GEO.DISTANCE",
              :args
              [{:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/const,
                :arg 1,
                :begin {:line 1, :column 14},
                :end {:line 1, :column 14}}
               {:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/const,
                :arg 2,
                :begin {:line 1, :column 16},
                :end {:line 1, :column 16}}]}
             (sut/parse "GEO.DISTANCE(1,2)")))))
