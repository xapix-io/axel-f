(ns axel-f.v2.parser
  (:require [axel-f.v2.parser :as sut]
            [clojure.test :as t]))

(t/deftest parse-tests

  (t/testing "parsing keywords"

    (t/is (= {:kind ::fncall,
              :f ::keyword,
              :arg :foo.bar/baz,
              :begin {:line 1, :column 1},
              :end {:line 1, :column 12}}
             (sut/parse ":foo.bar/baz"))))

  (t/testing "parsing constants"

    (t/testing "as a number"

      (t/is (= {:kind ::fncall
                :f ::const
                :arg 1
                :begin {:line 1 :column 1}
                :end {:line 1 :column 1}}
               (sut/parse "1"))))

    (t/testing "as a string"

      (t/is (= {:kind ::fncall
                :f ::const
                :arg "string"
                :begin {:line 1 :column 1}
                :end {:line 1 :column 8}}
               (sut/parse "'string'")))

      (t/is (= {:kind ::fncall
                :f ::const
                :arg "string"
                :begin {:line 1 :column 1}
                :end {:line 1 :column 8}}
               (sut/parse "\"string\"")))))

  (t/testing "parsing vector of constants"

    (t/is (= {:kind ::fncall,
              :f ::vector,
              :args
              [{:kind ::fncall,
                :f ::const,
                :arg 1,
                :begin {:line 1, :column 2},
                :end {:line 1, :column 2}}
               {:kind ::fncall,
                :f ::const,
                :arg 2,
                :begin {:line 1, :column 5},
                :end {:line 1, :column 5}}]}
             (sut/parse "[1, 2]")))

    (t/is (= {:kind ::fncall,
              :f ::vector,
              :args
              [{:kind ::fncall,
                :f ::const,
                :arg 1,
                :begin {:line 1, :column 2},
                :end {:line 1, :column 2}}
               {:kind ::fncall,
                :f ::const,
                :arg "foo",
                :begin {:line 1, :column 5},
                :end {:line 1, :column 9}}]}
             (sut/parse "[1, \"foo\"]"))))

  (t/testing "parsing simple arythmetic expressions"

    (t/is (= {:kind ::fncall,
              :f "-",
              :args
              [{:kind ::fncall,
                :f ::const,
                :arg 1,
                :begin {:line 1, :column 2},
                :end {:line 1, :column 2}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 2}}
             (sut/parse "-1")))

    (t/is (= {:kind ::fncall,
              :f "+",
              :args
              [{:kind ::fncall,
                :f ::const,
                :arg 1,
                :begin {:line 1, :column 1},
                :end {:line 1, :column 1}}
               {:kind ::fncall,
                :f ::const,
                :arg 1,
                :begin {:line 1, :column 5},
                :end {:line 1, :column 5}}]}
             (sut/parse "1 + 1")))

    (t/is (= {:kind ::fncall,
              :f "!",
              :args
              [{:kind ::fncall,
                :f ::const,
                :arg 1,
                :begin {:line 1, :column 2},
                :end {:line 1, :column 2}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 2}}
             (sut/parse "!1")))

    (t/is (= {:kind ::fncall,
              :f "%",
              :args
              [{:kind ::fncall,
                :f ::const,
                :arg 10,
                :begin {:line 1, :column 1},
                :end {:line 1, :column 2}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 3}}
             (sut/parse "10%"))))

  (t/testing "parsing reference expression"

    (t/is (= {:kind ::fncall,
              :f ::reference,
              :args
              [{:kind ::fncall,
                :f ::symbol,
                :arg "foo",
                :begin {:line 1, :column 1},
                :end {:line 1, :column 3}}
               {:kind ::fncall,
                :f ::reference,
                :args
                [{:kind ::fncall,
                  :f ::symbol,
                  :arg "bar",
                  :begin {:line 1, :column 5},
                  :end {:line 1, :column 7}}],
                :begin {:line 1, :column 5},
                :end {:line 1, :column 7}}
               {:kind ::fncall,
                :f ::reference,
                :args
                [{:kind ::fncall,
                  :f ::symbol,
                  :arg "baz",
                  :begin {:line 1, :column 9},
                  :end {:line 1, :column 11}}],
                :begin {:line 1, :column 9},
                :end {:line 1, :column 11}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 3}}
             (sut/parse "foo.bar.baz")))

    (t/is (= {:kind ::fncall,
              :f ::reference,
              :args
              [{:kind ::fncall,
                :f ::symbol,
                :arg "foo",
                :begin {:line 1, :column 1},
                :end {:line 1, :column 3}}
               {:kind ::fncall,
                :f ::nth,
                :arg
                {:kind ::fncall,
                 :f ::const,
                 :arg 1,
                 :begin {:line 1, :column 5},
                 :end {:line 1, :column 5}},
                :begin {:line 1, :column 4},
                :end {:line 1, :column 6}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 3}}
             (sut/parse "foo[1]")))

    (t/is (= {:kind ::fncall,
              :f ::reference,
              :args
              [{:kind ::fncall,
                :f ::symbol,
                :arg "foo",
                :begin {:line 1, :column 1},
                :end {:line 1, :column 3}}
               {:kind ::fncall,
                :f ::nth,
                :arg
                {:kind ::fncall,
                 :f "+",
                 :args
                 [{:kind ::fncall,
                   :f ::const,
                   :arg 1,
                   :begin {:line 1, :column 6},
                   :end {:line 1, :column 6}}
                  {:kind ::fncall,
                   :f ::const,
                   :arg 1,
                   :begin {:line 1, :column 8},
                   :end {:line 1, :column 8}}]},
                :begin {:line 1, :column 5},
                :end {:line 1, :column 9}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 3}}
             (sut/parse "foo.[1+1]")))

    (t/is (= {:kind ::fncall,
              :f ::reference,
              :args
              [{:kind ::fncall,
                :f ::symbol,
                :arg "foo",
                :begin {:line 1, :column 1},
                :end {:line 1, :column 3}}
               {:kind ::fncall,
                :f ::reference,
                :args
                [{:kind ::fncall,
                  :f ::symbol,
                  :arg "bar",
                  :begin {:line 1, :column 5},
                  :end {:line 1, :column 7}}],
                :begin {:line 1, :column 5},
                :end {:line 1, :column 7}}
               {:kind ::fncall,
                :f ::nth,
                :arg
                {:kind ::fncall,
                 :f ::ALL,
                 :arg nil},
                :begin {:line 1, :column 8},
                :end {:line 1, :column 10}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 3}}
             (sut/parse "foo.bar[*]")))

    (t/is (= {:kind ::fncall,
              :f ::reference,
              :args
              [{:kind ::fncall,
                :f ::symbol,
                :arg "foo",
                :begin {:line 1, :column 1},
                :end {:line 1, :column 3}}
               {:kind ::fncall,
                :f ::reference,
                :args
                [{:kind ::fncall,
                  :f ::symbol,
                  :arg "bar",
                  :begin {:line 1, :column 5},
                  :end {:line 1, :column 7}}],
                :begin {:line 1, :column 5},
                :end {:line 1, :column 7}}
               {:kind ::fncall,
                :f ::nth,
                :arg
                {:kind ::fncall,
                 :f ::ALL,
                 :arg nil},
                :begin {:line 1, :column 9},
                :end {:line 1, :column 11}}
               {:kind ::fncall,
                :f ::reference,
                :args
                [{:kind ::fncall,
                  :f ::symbol,
                  :arg "baz",
                  :begin {:line 1, :column 13},
                  :end {:line 1, :column 15}}],
                :begin {:line 1, :column 13},
                :end {:line 1, :column 15}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 3}}
             (sut/parse "foo.bar.[*].baz")))

    (t/is (= {:kind ::fncall,
              :f "MAP"
              :args
              [{:kind ::fncall,
                :f ::reference,
                :args
                [{:kind ::fncall,
                  :f ::symbol,
                  :arg "_",
                  :begin {:line 1, :column 5},
                  :end {:line 1, :column 5}}],
                :begin {:line 1, :column 5},
                :end {:line 1, :column 5}}
               {:kind ::fncall,
                :f ::vector,
                :args
                [{:kind ::fncall,
                  :f ::const,
                  :arg 1,
                  :begin {:line 1, :column 9},
                  :end {:line 1, :column 9}}
                 {:kind ::fncall,
                  :f ::const,
                  :arg 2,
                  :begin {:line 1, :column 12},
                  :end {:line 1, :column 12}}
                 {:kind ::fncall,
                  :f ::const,
                  :arg 3,
                  :begin {:line 1, :column 15},
                  :end {:line 1, :column 15}}]}]}
             (sut/parse "MAP(_, [1, 2, 3])")))))
