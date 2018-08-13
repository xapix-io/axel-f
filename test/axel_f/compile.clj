(ns axel-f.compile
  (:require  [clojure.test :as t]
             [axel-f.core :as sut]))

(t/deftest compile

  (t/testing "can parse normal formulas"

    (let [sample1 "1 + 1"
          sample2 "=1 + 1"
          sample3 "{=1 + 1}"]
      (let [result1 (sut/compile sample1)
            result2 (sut/compile sample2)
            result3 (sut/compile sample3)]

        (t/is (= result1 result2 result3 [:ADD_EXPR 1 1])))))

  (t/testing "can parse formulas with missing whitespaces"

    (let [sample1 "1+1"
          sample2 "1+ 1"
          sample3 "1 +1"
          sample4 " 1+1 "]
      (let [result1 (sut/compile sample1)
            result2 (sut/compile sample2)
            result3 (sut/compile sample3)
            result4 (sut/compile sample4)]

        (t/is (= result1 result2 result3 result4 [:ADD_EXPR 1 1])))))

  (t/testing "throw an error object with explanation if formula can't be parsed"

    (let [error-sample "1+"]

      (t/is (thrown? clojure.lang.ExceptionInfo #"Formula \"1+\" can't be parsed"
                     (sut/compile error-sample)))

      (let [failure (try
                      (sut/compile error-sample)
                      (catch clojure.lang.ExceptionInfo e
                        (ex-data e)))]

        (t/is (= {:index  2
                  :line   1
                  :column 3
                  :text   "1+"}
                 (select-keys failure
                              [:index :line :column :text])))
        (t/is (contains? failure :reason))
        (t/is (vector? (get failure :reason)))
        (t/is (every? #(contains? % :tag) (get failure :reason)))
        (t/is (every? #(contains? % :expecting) (get failure :reason))))))

  (t/testing "can compile"

    (t/testing "boolean constants"
      (t/are [x] (= true (sut/compile x))
        "TRUE"
        "True"
        "true")
      (t/are [x] (= false (sut/compile x))
        "FALSE"
        "False"
        "false"))

    (t/testing "number constants"
      (t/are [s n] (= n (sut/compile s))
        "1" 1
        "1.0" 1.0
        "1.e123" 1.E123))

    (t/testing "string constants"
      (t/is (= "foo" (sut/compile "\"foo\""))))

    (t/testing "all possible unary operators"

      (t/is (= (sut/compile "+1")
               [:SIGN_EXPR "+" 1]))
      (t/is (= (sut/compile "-1")
               [:SIGN_EXPR "-" 1]))
      (t/is (= (sut/compile "1%")
               [:PERCENT_EXPR 1])))

    (t/testing "all possible binary operators"

      (t/are [op token] (= (sut/compile (str "1" op "1"))
                           [token 1 1])
        "<"  :LESS_EXPR
        "<=" :LESS_OR_EQ_EXPR
        ">"  :MORE_EXPR
        ">=" :MORE_OR_EQ_EXPR
        "="  :EQ_EXPR
        "<>" :NOT_EQ_EXPR
        "&"  :CONCAT_EXPR
        "+"  :ADD_EXPR
        "-"  :SUB_EXPR
        "*"  :MULT_EXPR
        "/"  :DIV_EXPR
        "^"  :EXP_EXPR
        "&"  :CONCAT_EXPR))

    (t/testing "all possible functions deffined in grammar"

      (t/are [f] (= [:FNCALL f [1]] (sut/compile (str f "(1)")))
        "SUM"
        "IF"
        "MIN"
        "MAX"
        "ROUND"
        "LEN"
        "CONCATENATE"
        "AVERAGE"
        "AND"
        "OR"))

    (t/testing "data references"

      (t/is (= [:OBJREF "foo"]
               (sut/compile "foo")))

      (t/is (= [:OBJREF "foo" "bar"]
               (sut/compile "foo.bar")))

      (t/is (= [:OBJREF "foo" "bar" 1]
               (sut/compile "foo.bar[1]")))

      (t/is (= [:OBJREF "foo" "bar buz" 1]
               (sut/compile "foo.\"bar buz\"[1]"))))))

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
