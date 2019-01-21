(ns axel-f.operator-test
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [axel-f.core :as sut]
            [axel-f.lib :as lib]))

(t/deftest operator-precedence

  (t/testing "concat operator takes precedence over comparison operator"

    (t/are [comp-op token] (do (t/is (= {:kind :axel-f.v2.parser/fncall,
                                         :f "&",
                                         :args
                                         [{:kind :axel-f.v2.parser/fncall,
                                           :f comp-op,
                                           :args
                                           [{:kind :axel-f.v2.parser/fncall,
                                             :f :axel-f.v2.parser/const,
                                             :arg 1,
                                             :begin {:line 1, :column 1},
                                             :end {:line 1, :column 1}}
                                            {:kind :axel-f.v2.parser/fncall,
                                             :f :axel-f.v2.parser/const,
                                             :arg 1,
                                             :begin {:line 1, :column (+ 2 (count comp-op))},
                                             :end {:line 1, :column (+ 2 (count comp-op))}}]}
                                          {:kind :axel-f.v2.parser/fncall,
                                           :f :axel-f.v2.parser/const,
                                           :arg 1,
                                           :begin {:line 1, :column (+ 4 (count comp-op))},
                                           :end {:line 1, :column (+ 4 (count comp-op))}}]}
                                        (sut/compile (str "1" comp-op "1&1"))))
                               (t/is (= {:kind :axel-f.v2.parser/fncall,
                                         :f "&",
                                         :args
                                         [{:kind :axel-f.v2.parser/fncall,
                                           :f :axel-f.v2.parser/const,
                                           :arg 1,
                                           :begin {:line 1, :column 1},
                                           :end {:line 1, :column 1}}
                                          {:kind :axel-f.v2.parser/fncall,
                                           :f comp-op,
                                           :args
                                           [{:kind :axel-f.v2.parser/fncall,
                                             :f :axel-f.v2.parser/const,
                                             :arg 1,
                                             :begin {:line 1, :column 3},
                                             :end {:line 1, :column 3}}
                                            {:kind :axel-f.v2.parser/fncall,
                                             :f :axel-f.v2.parser/const,
                                             :arg 1,
                                             :begin {:line 1, :column (+ 4 (count comp-op))},
                                             :end {:line 1, :column (+ 4 (count comp-op))}}]}]}
                                        (sut/compile (str "1&1" comp-op "1")))))

      "<"  :LESS_EXPR
      "<=" :LESS_OR_EQ_EXPR
      ">"  :MORE_EXPR
      ">=" :MORE_OR_EQ_EXPR
      "="  :EQ_EXPR
      "<>" :NOT_EQ_EXPR))

  (t/testing "additive operator takes precedence over comparison operator"

    (t/are [add-op add-token]
        (t/are [comp-op comp-token] (do (t/is (= {:kind :axel-f.v2.parser/fncall,
                                                  :f comp-op,
                                                  :args
                                                  [{:kind :axel-f.v2.parser/fncall,
                                                    :f add-op,
                                                    :args
                                                    [{:kind :axel-f.v2.parser/fncall,
                                                      :f :axel-f.v2.parser/const,
                                                      :arg 1,
                                                      :begin {:line 1, :column 1},
                                                      :end {:line 1, :column 1}}
                                                     {:kind :axel-f.v2.parser/fncall,
                                                      :f :axel-f.v2.parser/const,
                                                      :arg 1,
                                                      :begin {:line 1, :column 3},
                                                      :end {:line 1, :column 3}}]}
                                                   {:kind :axel-f.v2.parser/fncall,
                                                    :f :axel-f.v2.parser/const,
                                                    :arg 1,
                                                    :begin {:line 1, :column (+ 4 (count comp-op))},
                                                    :end {:line 1, :column (+ 4 (count comp-op))}}]}
                                                 (sut/compile (str "1" add-op "1" comp-op "1"))))
                                        (t/is (= {:kind :axel-f.v2.parser/fncall,
                                                  :f comp-op,
                                                  :args
                                                  [{:kind :axel-f.v2.parser/fncall,
                                                    :f :axel-f.v2.parser/const,
                                                    :arg 1,
                                                    :begin {:line 1, :column 1},
                                                    :end {:line 1, :column 1}}
                                                   {:kind :axel-f.v2.parser/fncall,
                                                    :f add-op,
                                                    :args
                                                    [{:kind :axel-f.v2.parser/fncall,
                                                      :f :axel-f.v2.parser/const,
                                                      :arg 1,
                                                      :begin {:line 1, :column (+ 2 (count comp-op))},
                                                      :end {:line 1, :column (+ 2 (count comp-op))}}
                                                     {:kind :axel-f.v2.parser/fncall,
                                                      :f :axel-f.v2.parser/const,
                                                      :arg 1,
                                                      :begin {:line 1, :column (+ 4 (count comp-op))},
                                                      :end {:line 1, :column (+ 4 (count comp-op))}}]}]}
                                                 (sut/compile (str "1" comp-op "1" add-op "1")))))
          "<"  :LESS_EXPR
          "<=" :LESS_OR_EQ_EXPR
          ">"  :MORE_EXPR
          ">=" :MORE_OR_EQ_EXPR
          "="  :EQ_EXPR
          "<>" :NOT_EQ_EXPR)
      "+"  :ADD_EXPR
      "-"  :SUB_EXPR))

  (t/testing "multiplicative operator takes precedence over comparison operator"

    (t/are [mult-op mult-token]
        (t/are [comp-op comp-token] (do (t/is (= {:kind :axel-f.v2.parser/fncall,
                                                  :f comp-op,
                                                  :args
                                                  [{:kind :axel-f.v2.parser/fncall,
                                                    :f mult-op,
                                                    :args
                                                    [{:kind :axel-f.v2.parser/fncall,
                                                      :f :axel-f.v2.parser/const,
                                                      :arg 1,
                                                      :begin {:line 1, :column 1},
                                                      :end {:line 1, :column 1}}
                                                     {:kind :axel-f.v2.parser/fncall,
                                                      :f :axel-f.v2.parser/const,
                                                      :arg 1,
                                                      :begin {:line 1, :column 3},
                                                      :end {:line 1, :column 3}}]}
                                                   {:kind :axel-f.v2.parser/fncall,
                                                    :f :axel-f.v2.parser/const,
                                                    :arg 1,
                                                    :begin {:line 1, :column (+ 4 (count comp-op))},
                                                    :end {:line 1, :column (+ 4 (count comp-op))}}]}
                                                 (sut/compile (str "1" mult-op "1" comp-op "1"))))
                                        (t/is (= {:kind :axel-f.v2.parser/fncall,
                                                  :f comp-op,
                                                  :args
                                                  [{:kind :axel-f.v2.parser/fncall,
                                                    :f :axel-f.v2.parser/const,
                                                    :arg 1,
                                                    :begin {:line 1, :column 1},
                                                    :end {:line 1, :column 1}}
                                                   {:kind :axel-f.v2.parser/fncall,
                                                    :f mult-op,
                                                    :args
                                                    [{:kind :axel-f.v2.parser/fncall,
                                                      :f :axel-f.v2.parser/const,
                                                      :arg 1,
                                                      :begin {:line 1, :column (+ 2 (count comp-op))},
                                                      :end {:line 1, :column (+ 2 (count comp-op))}}
                                                     {:kind :axel-f.v2.parser/fncall,
                                                      :f :axel-f.v2.parser/const,
                                                      :arg 1,
                                                      :begin {:line 1, :column (+ 4 (count comp-op))},
                                                      :end {:line 1, :column (+ 4 (count comp-op))}}]}]}
                                                 (sut/compile (str "1" comp-op "1" mult-op "1")))))
          "<"  :LESS_EXPR
          "<=" :LESS_OR_EQ_EXPR
          ">"  :MORE_EXPR
          ">=" :MORE_OR_EQ_EXPR
          "="  :EQ_EXPR
          "<>" :NOT_EQ_EXPR)
      "*"  :MULT_EXPR
      "/"  :DIV_EXPR))

  (t/testing "exponential operator takes precedence over comparison operator"

    (t/are [comp-op token] (do (t/is (= {:kind :axel-f.v2.parser/fncall,
                                         :f comp-op,
                                         :args
                                         [{:kind :axel-f.v2.parser/fncall,
                                           :f "^",
                                           :args
                                           [{:kind :axel-f.v2.parser/fncall,
                                             :f :axel-f.v2.parser/const,
                                             :arg 1,
                                             :begin {:line 1, :column 1},
                                             :end {:line 1, :column 1}}
                                            {:kind :axel-f.v2.parser/fncall,
                                             :f :axel-f.v2.parser/const,
                                             :arg 1,
                                             :begin {:line 1, :column 3},
                                             :end {:line 1, :column 3}}]}
                                          {:kind :axel-f.v2.parser/fncall,
                                           :f :axel-f.v2.parser/const,
                                           :arg 1,
                                           :begin {:line 1, :column (+ 4 (count comp-op))},
                                           :end {:line 1, :column (+ 4 (count comp-op))}}]}
                                        (sut/compile (str "1^1" comp-op "1"))))
                               (t/is (= {:kind :axel-f.v2.parser/fncall,
                                         :f comp-op,
                                         :args
                                         [{:kind :axel-f.v2.parser/fncall,
                                           :f :axel-f.v2.parser/const,
                                           :arg 1,
                                           :begin {:line 1, :column 1},
                                           :end {:line 1, :column 1}}
                                          {:kind :axel-f.v2.parser/fncall,
                                           :f "^",
                                           :args
                                           [{:kind :axel-f.v2.parser/fncall,
                                             :f :axel-f.v2.parser/const,
                                             :arg 1,
                                             :begin {:line 1, :column (+ 2 (count comp-op))},
                                             :end {:line 1, :column (+ 2 (count comp-op))}}
                                            {:kind :axel-f.v2.parser/fncall,
                                             :f :axel-f.v2.parser/const,
                                             :arg 1,
                                             :begin {:line 1, :column (+ 4 (count comp-op))},
                                             :end {:line 1, :column (+ 4 (count comp-op))}}]}]}
                                        (sut/compile (str "1" comp-op "1^1")))))
      "<"  :LESS_EXPR
      "<=" :LESS_OR_EQ_EXPR
      ">"  :MORE_EXPR
      ">=" :MORE_OR_EQ_EXPR
      "="  :EQ_EXPR
      "<>" :NOT_EQ_EXPR))

  (t/testing "additive operator takes precedence over concat operator"

    (t/are [add-op token] (do (t/is (= {:kind :axel-f.v2.parser/fncall,
                                        :f "&",
                                        :args
                                        [{:kind :axel-f.v2.parser/fncall,
                                          :f :axel-f.v2.parser/const,
                                          :arg 1,
                                          :begin {:line 1, :column 1},
                                          :end {:line 1, :column 1}}
                                         {:kind :axel-f.v2.parser/fncall,
                                          :f add-op,
                                          :args
                                          [{:kind :axel-f.v2.parser/fncall,
                                            :f :axel-f.v2.parser/const,
                                            :arg 1,
                                            :begin {:line 1, :column 3},
                                            :end {:line 1, :column 3}}
                                           {:kind :axel-f.v2.parser/fncall,
                                            :f :axel-f.v2.parser/const,
                                            :arg 1,
                                            :begin {:line 1, :column 5},
                                            :end {:line 1, :column 5}}]}]}
                                       (sut/compile (str "1&1" add-op "1"))))
                              (t/is (= {:kind :axel-f.v2.parser/fncall,
                                        :f "&",
                                        :args
                                        [{:kind :axel-f.v2.parser/fncall,
                                          :f add-op,
                                          :args
                                          [{:kind :axel-f.v2.parser/fncall,
                                            :f :axel-f.v2.parser/const,
                                            :arg 1,
                                            :begin {:line 1, :column 1},
                                            :end {:line 1, :column 1}}
                                           {:kind :axel-f.v2.parser/fncall,
                                            :f :axel-f.v2.parser/const,
                                            :arg 1,
                                            :begin {:line 1, :column 3},
                                            :end {:line 1, :column 3}}]}
                                         {:kind :axel-f.v2.parser/fncall,
                                          :f :axel-f.v2.parser/const,
                                          :arg 1,
                                          :begin {:line 1, :column 5},
                                          :end {:line 1, :column 5}}]}
                                       (sut/compile (str "1" add-op "1&1")))))
      "+"  :ADD_EXPR
      "-"  :SUB_EXPR))

  (t/testing "multiplicative operator takes precedence over concat operator"

    (t/are [mult-op token] (do (t/is (= {:kind :axel-f.v2.parser/fncall,
                                         :f "&",
                                         :args
                                         [{:kind :axel-f.v2.parser/fncall,
                                           :f :axel-f.v2.parser/const,
                                           :arg 1,
                                           :begin {:line 1, :column 1},
                                           :end {:line 1, :column 1}}
                                          {:kind :axel-f.v2.parser/fncall,
                                           :f mult-op,
                                           :args
                                           [{:kind :axel-f.v2.parser/fncall,
                                             :f :axel-f.v2.parser/const,
                                             :arg 1,
                                             :begin {:line 1, :column 3},
                                             :end {:line 1, :column 3}}
                                            {:kind :axel-f.v2.parser/fncall,
                                             :f :axel-f.v2.parser/const,
                                             :arg 1,
                                             :begin {:line 1, :column 5},
                                             :end {:line 1, :column 5}}]}]}
                                       (sut/compile (str "1&1" mult-op "1"))))
                               (t/is (= {:kind :axel-f.v2.parser/fncall,
                                         :f "&",
                                         :args
                                         [{:kind :axel-f.v2.parser/fncall,
                                           :f mult-op,
                                           :args
                                           [{:kind :axel-f.v2.parser/fncall,
                                             :f :axel-f.v2.parser/const,
                                             :arg 1,
                                             :begin {:line 1, :column 1},
                                             :end {:line 1, :column 1}}
                                            {:kind :axel-f.v2.parser/fncall,
                                             :f :axel-f.v2.parser/const,
                                             :arg 1,
                                             :begin {:line 1, :column 3},
                                             :end {:line 1, :column 3}}]}
                                          {:kind :axel-f.v2.parser/fncall,
                                           :f :axel-f.v2.parser/const,
                                           :arg 1,
                                           :begin {:line 1, :column 5},
                                           :end {:line 1, :column 5}}]}
                                       (sut/compile (str "1" mult-op "1&1")))))
      "*"  :MULT_EXPR
      "/"  :DIV_EXPR))

  (t/testing "exponential operator takes precedence over concat operator"

    (t/is (= {:kind :axel-f.v2.parser/fncall,
              :f "&",
              :args
              [{:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/const,
                :arg 1,
                :begin {:line 1, :column 1},
                :end {:line 1, :column 1}}
               {:kind :axel-f.v2.parser/fncall,
                :f "^",
                :args
                [{:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/const,
                  :arg 1,
                  :begin {:line 1, :column 3},
                  :end {:line 1, :column 3}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/const,
                  :arg 1,
                  :begin {:line 1, :column 5},
                  :end {:line 1, :column 5}}]}]}
             (sut/compile "1&1^1")))
    (t/is (= {:kind :axel-f.v2.parser/fncall,
              :f "&",
              :args
              [{:kind :axel-f.v2.parser/fncall,
                :f "^",
                :args
                [{:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/const,
                  :arg 1,
                  :begin {:line 1, :column 1},
                  :end {:line 1, :column 1}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/const,
                  :arg 1,
                  :begin {:line 1, :column 3},
                  :end {:line 1, :column 3}}]}
               {:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/const,
                :arg 1,
                :begin {:line 1, :column 5},
                :end {:line 1, :column 5}}]}
             (sut/compile "1^1&1"))))

  (t/testing "multiplicative operators takes precedence over additive operators"

    (t/are [mult-op mult-token]
        (t/are [add-op add-token] (do (t/is (= {:kind :axel-f.v2.parser/fncall,
                                                :f add-op,
                                                :args
                                                [{:kind :axel-f.v2.parser/fncall,
                                                  :f mult-op,
                                                  :args
                                                  [{:kind :axel-f.v2.parser/fncall,
                                                    :f :axel-f.v2.parser/const,
                                                    :arg 1,
                                                    :begin {:line 1, :column 1},
                                                    :end {:line 1, :column 1}}
                                                   {:kind :axel-f.v2.parser/fncall,
                                                    :f :axel-f.v2.parser/const,
                                                    :arg 1,
                                                    :begin {:line 1, :column 3},
                                                    :end {:line 1, :column 3}}]}
                                                 {:kind :axel-f.v2.parser/fncall,
                                                  :f :axel-f.v2.parser/const,
                                                  :arg 1,
                                                  :begin {:line 1, :column 5},
                                                  :end {:line 1, :column 5}}]}
                                               (sut/compile (str "1" mult-op "1" add-op "1"))))
                                      (t/is (= {:kind :axel-f.v2.parser/fncall,
                                                :f add-op,
                                                :args
                                                [{:kind :axel-f.v2.parser/fncall,
                                                  :f :axel-f.v2.parser/const,
                                                  :arg 1,
                                                  :begin {:line 1, :column 1},
                                                  :end {:line 1, :column 1}}
                                                 {:kind :axel-f.v2.parser/fncall,
                                                  :f mult-op,
                                                  :args
                                                  [{:kind :axel-f.v2.parser/fncall,
                                                    :f :axel-f.v2.parser/const,
                                                    :arg 1,
                                                    :begin {:line 1, :column 3},
                                                    :end {:line 1, :column 3}}
                                                   {:kind :axel-f.v2.parser/fncall,
                                                    :f :axel-f.v2.parser/const,
                                                    :arg 1,
                                                    :begin {:line 1, :column 5},
                                                    :end {:line 1, :column 5}}]}]}
                                               (sut/compile (str "1" add-op "1" mult-op "1")))))
          "+"  :ADD_EXPR
          "-"  :SUB_EXPR)
      "*"  :MULT_EXPR
      "/"  :DIV_EXPR))

  (t/testing "exponential operator takes precedence over multiplicative operator"

    (t/are [mult-op token] (do (t/is (= {:kind :axel-f.v2.parser/fncall,
                                         :f mult-op,
                                         :args
                                         [{:kind :axel-f.v2.parser/fncall,
                                           :f "^",
                                           :args
                                           [{:kind :axel-f.v2.parser/fncall,
                                             :f :axel-f.v2.parser/const,
                                             :arg 1,
                                             :begin {:line 1, :column 1},
                                             :end {:line 1, :column 1}}
                                            {:kind :axel-f.v2.parser/fncall,
                                             :f :axel-f.v2.parser/const,
                                             :arg 1,
                                             :begin {:line 1, :column 3},
                                             :end {:line 1, :column 3}}]}
                                          {:kind :axel-f.v2.parser/fncall,
                                           :f :axel-f.v2.parser/const,
                                           :arg 1,
                                           :begin {:line 1, :column 5},
                                           :end {:line 1, :column 5}}]}
                                        (sut/compile (str "1^1" mult-op "1"))))
                               (t/is (= {:kind :axel-f.v2.parser/fncall,
                                         :f mult-op,
                                         :args
                                         [{:kind :axel-f.v2.parser/fncall,
                                           :f :axel-f.v2.parser/const,
                                           :arg 1,
                                           :begin {:line 1, :column 1},
                                           :end {:line 1, :column 1}}
                                          {:kind :axel-f.v2.parser/fncall,
                                           :f "^",
                                           :args
                                           [{:kind :axel-f.v2.parser/fncall,
                                             :f :axel-f.v2.parser/const,
                                             :arg 1,
                                             :begin {:line 1, :column 3},
                                             :end {:line 1, :column 3}}
                                            {:kind :axel-f.v2.parser/fncall,
                                             :f :axel-f.v2.parser/const,
                                             :arg 1,
                                             :begin {:line 1, :column 5},
                                             :end {:line 1, :column 5}}]}]}
                                        (sut/compile (str "1" mult-op "1^1")))))
      "*"  :MULT_EXPR
      "/"  :DIV_EXPR))

  (t/testing "grouping with parenthesis changes operator precedence"

    (t/is (= {:kind :axel-f.v2.parser/fncall,
              :f "*",
              :args
              [{:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/const,
                :arg 1,
                :begin {:line 1, :column 1},
                :end {:line 1, :column 1}}
               {:kind :axel-f.v2.parser/fncall,
                :f "<",
                :args
                [{:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/const,
                  :arg 1,
                  :begin {:line 1, :column 4},
                  :end {:line 1, :column 4}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/const,
                  :arg 1,
                  :begin {:line 1, :column 6},
                  :end {:line 1, :column 6}}]}]}
             (sut/compile "1*(1<1)")))))

(t/deftest sign-operator

  (t/testing "prefix operators converted into application form"

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
             (sut/compile "-1")))
    (t/is (=  {:kind :axel-f.v2.parser/fncall,
               :f "+",
               :args
               [{:kind :axel-f.v2.parser/fncall,
                 :f :axel-f.v2.parser/const,
                 :arg false,
                 :begin {:line 1, :column 2},
                 :end {:line 1, :column 6}}],
               :begin {:line 1, :column 1},
               :end {:line 1, :column 6}}
              (sut/compile "+False")))
    (t/is (= {:kind :axel-f.v2.parser/fncall,
              :f "-",
              :args
              [{:kind :axel-f.v2.parser/fncall,
                :f "-",
                :args
                [{:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/const,
                  :arg true,
                  :begin {:line 1, :column 3},
                  :end {:line 1, :column 6}}],
                :begin {:line 1, :column 2},
                :end {:line 1, :column 6}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 6}}
             (sut/compile "--TRUE")))
    (t/is (= {:kind :axel-f.v2.parser/fncall,
              :f "-",
              :args
              [{:kind :axel-f.v2.parser/fncall,
                :f "-",
                :args
                [{:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/const,
                  :arg true,
                  :begin {:line 1, :column 3},
                  :end {:line 1, :column 6}}],
                :begin {:line 1, :column 2},
                :end {:line 1, :column 6}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 6}}
             (sut/compile "--TRUE")))

    (t/is (= {:kind :axel-f.v2.parser/fncall,
              :f "-",
              :args
              [{:kind :axel-f.v2.parser/fncall,
                :f "-",
                :args
                [{:kind :axel-f.v2.parser/fncall,
                  :f "<",
                  :args
                  [{:kind :axel-f.v2.parser/fncall,
                    :f :axel-f.v2.parser/const,
                    :arg 1,
                    :begin {:line 1, :column 4},
                    :end {:line 1, :column 4}}
                   {:kind :axel-f.v2.parser/fncall,
                    :f :axel-f.v2.parser/const,
                    :arg 0,
                    :begin {:line 1, :column 6},
                    :end {:line 1, :column 6}}]}],
                :begin {:line 1, :column 2},
                :end nil}],
              :begin {:line 1, :column 1},
              :end nil}
             (sut/compile "--(1<0)"))))

  (t/testing "double negative can be used to coerce boolean to integer"
    (t/is (= 1 (sut/run "--TRUE")))
    (t/is (= 0 (sut/run "--FALSE")))
    (t/is (= 1 (sut/run "--(1>0)")))))

(t/deftest percent-operator

  (t/testing "postfix operator converted in application form"

    (t/is (= {:kind :axel-f.v2.parser/fncall,
              :f "%",
              :args
              [{:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/const,
                :arg 2.4,
                :begin {:line 1, :column 1},
                :end {:line 1, :column 3}}],
              :begin {:line 1, :column 1},
              :end {:line 1, :column 4}}
             (sut/compile "2.4%"))))

  (t/testing "percent operator evaluated into float number"

    (t/is (lib/fuzzy= 0.0001 0.024 (sut/run "2.4%")))))
