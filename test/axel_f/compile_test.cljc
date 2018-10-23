(ns axel-f.compile-test
  (:require  #?(:clj [clojure.test :as t]
                :cljs [cljs.test :as t :include-macros true])
             [clojure.string :as string]
             [axel-f.core :as sut]))

(t/deftest compile-tests

  (t/testing "can parse normal formulas"

    (let [sample1 "1 + 1"
          sample2 "=1 + 1"]
      (let [result1 (sut/compile sample1)
            result2 (sut/compile sample2)]

        (t/is (= result1 result2 [:ADD_EXPR 1 1])))))

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

    (let [error-sample "1("]

      (t/is (thrown? #?(:clj clojure.lang.ExceptionInfo
                        :cljs ExceptionInfo)
                     #"Formula \"1\(\" can't be parsed"
                     (sut/compile error-sample)))

      (let [failure (try
                      (sut/compile error-sample)
                      (catch #?(:clj clojure.lang.ExceptionInfo
                                :cljs ExceptionInfo)
                          e
                        (:data (ex-data e))))]

        (t/is (= {:index  1
                  :line   1
                  :column 2
                  :text   "1("}
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
      (t/is (= [:STRING "foo"] (sut/compile "\"foo\""))))

    (t/testing "all possible unary operators"

      (t/is (= (sut/compile "+1")
               1))
      (t/is (= (sut/compile "-1")
               -1))
      (t/is (= (sut/compile "(1 + 1)%")
               [:PERCENT_EXPR [:ADD_EXPR 1 1]])))

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

    (t/testing "all possible functions defined in grammar"

      (t/are [f] (= [:FNCALL f [1]] (sut/compile (str f "(1)")))
        "SUM"
        "IF"
        "MIN"
        "MAX"
        "ROUND"
        "COUNT"
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

      (t/is (= [:OBJREF "foo" [:STRING "bar buz"] 1]
               (sut/compile "foo.\"bar buz\"[1]")))))

  (t/testing "can not compile"

    (t/testing "reserved strings"

      (t/is (thrown? #?(:clj Exception
                        :cljs js/Error)
                     #"String \"OBJREF\" is reserved."
                     (sut/compile "\"OBJREF\"")))

      (for [x sut/reserved-tokens]
        (t/is (thrown? #?(:clj Exception
                          :cljs js/Error)
                       (re-pattern (str "String " x " is reserved."))
                       (sut/compile (str "\"" x "\"")))))))

  (t/testing "can accept custom transformations"

    (letfn [(objref-custom-transform [& fields]
              (cons :OBJREF
                    (reduce (fn [acc [f n]]
                              (let [n (if (and (= "header" f) (string? n))
                                        (string/lower-case n)
                                        n)]
                                (conj (or (not-empty acc) [f])
                                      n)))
                            []
                            (partition 2 1 fields))))]

      (t/is (= [:OBJREF "header" "Content-Type"]
               (sut/compile "header.Content-Type")))
      (t/is (= [:OBJREF "header" "content-type"]
               (sut/compile "header.Content-Type" :OBJREF objref-custom-transform)))
      (t/is (= [:OBJREF "request" "header" "content-type"]
               (sut/compile "request.header.Content-Type" :OBJREF objref-custom-transform))))))
