(ns axel-f.operator-test
  (:require  [clojure.test :as t]
             [axel-f.core :as sut]))

(t/deftest operator-precedence

  (t/testing "concat operator takes precedence over comparison operator"

    (t/are [comp-op token] (do (t/is (= [token 1 [:CONCAT_EXPR 1 1]]
                                        (sut/compile (str "1" comp-op "1&1"))))
                               (t/is (= [token [:CONCAT_EXPR 1 1] 1]
                                        (sut/compile (str "1&1" comp-op "1")))))

      "<"  :LESS_EXPR
      "<=" :LESS_OR_EQ_EXPR
      ">"  :MORE_EXPR
      ">=" :MORE_OR_EQ_EXPR
      "="  :EQ_EXPR
      "<>" :NOT_EQ_EXPR))

  (t/testing "additive operator takes precedence over comparison operator"

    (t/are [add-op add-token]
        (t/are [comp-op comp-token] (do (t/is (= [comp-token [add-token 1 1] 1]
                                                 (sut/compile (str "1" add-op "1" comp-op "1"))))
                                        (t/is (= [comp-token 1 [add-token 1 1]]
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
        (t/are [comp-op comp-token] (do (t/is (= [comp-token [mult-token 1 1] 1]
                                                 (sut/compile (str "1" mult-op "1" comp-op "1"))))
                                        (t/is (= [comp-token 1 [mult-token 1 1]]
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

    (t/are [comp-op token] (do (t/is (= [token [:EXP_EXPR 1 1] 1]
                                        (sut/compile (str "1^1" comp-op "1"))))
                               (t/is (= [token 1 [:EXP_EXPR 1 1]]
                                        (sut/compile (str "1" comp-op "1^1")))))
      "<"  :LESS_EXPR
      "<=" :LESS_OR_EQ_EXPR
      ">"  :MORE_EXPR
      ">=" :MORE_OR_EQ_EXPR
      "="  :EQ_EXPR
      "<>" :NOT_EQ_EXPR))

  (t/testing "additive operator takes precedence over concat operator"

    (t/are [add-op token] (do (t/is (= [:CONCAT_EXPR 1 [token 1 1]]
                                       (sut/compile (str "1&1" add-op "1"))))
                              (t/is (= [:CONCAT_EXPR [token 1 1] 1]
                                       (sut/compile (str "1" add-op "1&1")))))
      "+"  :ADD_EXPR
      "-"  :SUB_EXPR))

  (t/testing "multiplicative operator takes precedence over concat operator"

    (t/are [mult-op token] (do (t/is (= [:CONCAT_EXPR 1 [token 1 1]]
                                       (sut/compile (str "1&1" mult-op "1"))))
                              (t/is (= [:CONCAT_EXPR [token 1 1] 1]
                                       (sut/compile (str "1" mult-op "1&1")))))
      "*"  :MULT_EXPR
      "/"  :DIV_EXPR))

  (t/testing "exponential operator takes precedence over concat operator"

    (t/is (= [:CONCAT_EXPR 1 [:EXP_EXPR 1 1]]
             (sut/compile "1&1^1")))
    (t/is (= [:CONCAT_EXPR [:EXP_EXPR 1 1] 1]
             (sut/compile "1^1&1"))))

  (t/testing "multiplicative operators takes precedence over additive operators"

    (t/are [mult-op mult-token]
        (t/are [add-op add-token] (do (t/is (= [add-token [mult-token 1 1] 1]
                                               (sut/compile (str "1" mult-op "1" add-op "1"))))
                                      (t/is (= [add-token 1 [mult-token 1 1]]
                                               (sut/compile (str "1" add-op "1" mult-op "1")))))
          "+"  :ADD_EXPR
          "-"  :SUB_EXPR)
      "*"  :MULT_EXPR
      "/"  :DIV_EXPR))

  (t/testing "exponential operator takes precedence over multiplicative operator"

    (t/are [mult-op token] (do (t/is (= [token [:EXP_EXPR 1 1] 1]
                                        (sut/compile (str "1^1" mult-op "1"))))
                               (t/is (= [token 1 [:EXP_EXPR 1 1]]
                                        (sut/compile (str "1" mult-op "1^1")))))
      "*"  :MULT_EXPR
      "/"  :DIV_EXPR))

  (t/testing "grouping with parenthesis changes operator precedence"

    (t/is (= [:MULT_EXPR 1 [:LESS_EXPR 1 1]]
             (sut/compile "1*(1<1)")))))
