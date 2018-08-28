(ns axel-f.functions-test
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [axel-f.core :as sut]))


(t/deftest nested-function-test
  (t/testing "nested function calls"
    (let [[fncall & _] (last (sut/compile "SUM(SUM(1,2,3),5)") )]
      (t/is (= [:FNCALL "SUM" [1 2 3]]
               fncall)))))

(t/deftest vector-args-test
  (t/testing "vectors can be passed as arguments"

    (t/is (= [:FNCALL "SUM" [[:VECTOR 1 2 3] 4]]
             (sut/compile "SUM({1,2,3}, 4)")))

    (t/is (= 10 (sut/run "SUM({1,2,3},4)"))
          (= 15 (sut/run "SUM({1,2,3}, {4,5})")))))

(t/deftest function-arguments-test

  (t/testing "function arguments parsed as vector"

    (t/is (vector? (last (sut/compile "SUM()"))))
    (t/is (empty? (last (sut/compile "SUM()"))))

    (t/is (= [1 2 3]
             (last (sut/compile "SUM(1,2,3)"))
             (last (sut/compile "SUM(1, 2, 3)"))
             (last (sut/compile "SUM( 1, 2,3 )")))))
  )

(t/deftest ROUND

  (t/testing "ROUND function should work as expected"

    (t/are [x y] (t/is (= x (sut/run (str "ROUND(123123.123, " y ")"))))
      123000     -3
      123100     -2
      123120     -1
      123123     0
      123123.1   1
      123123.12  2
      123123.123 3)

    (t/is (= 123123 (sut/run "ROUND(123123.123)")))

    (t/is (= {:type "#VALUE!"
              :reason
              "Function ROUND parameter 1 expects number values. But 'foo' is a text and cannot be coerced to a number."}
             (sut/run "ROUND(\"foo\")")))

    (t/is (= {:type "#VALUE!"
              :reason
              "Function ROUND parameter 2 expects number values. But 'foo' is a text and cannot be coerced to a number."}
             (sut/run "ROUND(123123.123, \"foo\")")))))

(t/deftest COUNT

  (t/testing "COUNT function should work as expected"

    (t/are [x y] (t/is (= x (sut/run (str "COUNT(" y ")") {:foo [1 2 3]})))
      0 "\"foo\""
      1 "{\"foo\", 1}"
      1 "{1}"
      3 "foo")))

(t/deftest MIN

  (t/testing "MIN function should work as expected"

    (t/are [x y] (t/is (= x (sut/run (str "MIN(" y ")") {:foo [1 2 3]})))
      1 "{1}"
      1 "foo")

    (t/is (= {:type "#VALUE!"
              :reason "Function MIN parameter expects number values. But 'foo' is a text and cannot be coerced to a number."}
             (sut/run "MIN(\"foo\")")))))

(t/deftest MAX

  (t/testing "MAX function should work as expected"

    (t/are [x y] (t/is (= x (sut/run (str "MAX(" y ")") {:foo [1 2 3]})))
      1 "{1}"
      3 "foo")

    (t/is (= {:type "#VALUE!"
              :reason "Function MAX parameter expects number values. But 'foo' is a text and cannot be coerced to a number."}
             (sut/run "MAX(\"foo\")")))))

(t/deftest SUM

  (t/testing "SUM function should work as expected"

    (t/are [x y] (t/is (= x (sut/run (str "SUM(" y ")") {:foo [1 2 3]
                                                         :bar ["2.1" 3.9]})))
      6 "foo"
      7 "foo, 1"
      6 "{1,2,3}"
      1 "'1'"
      3 "{'1', '2'}"
      6.0 "bar"
      1 "TRUE, FALSE")

    (t/is (= {:type "#VALUE!"
              :reason "Function SUM parameter expects number values. But 'foo' is a text and cannot be coerced to a number."}
             (sut/run "SUM(\"foo\")")))))

(t/deftest CONCATENATE

  (t/testing "CONCATENATE function should work as expected"

    (t/are [x y] (t/is (= x (sut/run (str "CONCATENATE(" y ")") {:foo [1 2 3]})))
      "1" "{1}"
      "2" "foo[1]"
      "123" "foo")))

(t/deftest IF

  (t/testing "IF function should work as expected"

    (t/are [x y] (t/is (= x (sut/run (str "IF(" y ")") {:foo [1 2 3]})))
      0 "FALSE, 1, 0"
      1 "TRUE, 1, 0"
      1 "True, 1"
      nil "False, 1")))

(t/deftest AVERAGE

  (t/testing "AVERAGE function should work as expected"

    (t/are [x y] (t/is (= x (sut/run (str "AVERAGE(" y ")") {:foo [1 2 3] :bar []})))
      2 "{1,2,3}"
      nil "bar"
      2 "foo"
      4/3 "1, 2, TRUE")))

(t/deftest AND

  (t/testing "AND function should work as expected"

    (t/are [x y] (t/is (= x (sut/run (str "AND(" y ")") {:foo [1 2 3] :bar []})))
      true "1 > 0, 2 > 1"
      false "1 > 0, 2 < 1"
      true "COUNT(foo)"
      false "AVERAGE(foo) < COUNT(bar)")))

(t/deftest OR

  (t/testing "OR function should work as expected"

    (t/are [x y] (t/is (= x (sut/run (str "OR(" y ")") {:foo [1 2 3] :bar []})))
      true "1 > 0, 2 > 1"
      true "1 > 0, 2 < 1"
      true "COUNT(foo)"
      false "AVERAGE(foo) < COUNT(bar)")))

(t/deftest NOT

  (t/testing "NOT function should work as expected"

    (t/are [x y] (t/is (= x (sut/run (str "NOT(" y ")") {:foo [1 2 3]})))
      true "1 > 2"
      false "True"
      true "False")))

(t/deftest OBJREF

  (t/testing "OBJREF function should work as expected"

    (t/are [x y] (t/is (= x (sut/run (str "OBJREF(" y ")") {:foo [1 2 3] :bar []})))
      2 "\"foo\", 1"
      [] "\"bar\""
      3 "CONCATENATE(\"f\", \"o\", \"o\"), 2"
      nil "\"baz\"")))

(t/deftest code-function-test
  (t/testing "CODE function"
    (t/is (= 65
             (sut/run "CODE(\"A\")")))
    (t/is (= 1000
             (sut/run "CODE(\"Ϩ\")"))))
  )

(t/deftest concatenate-function-test
  (t/testing "CONCATENATE function"
    (t/is (= "hello world"
             (sut/run "CONCATENATE(\"hello\", \" \", \"world\")")))
    (t/is (= "hello world"
             (sut/run "CONCATENATE({\"hello\", \" \", \"world\"})")))
    (t/is (= "1hello"
             (sut/run "CONCATENATE(1, \"hello\",)")))
    (t/is (= "TRUEyes"
             (sut/run "CONCATENATE(true, \"yes\")")))
    (t/is (= "FALSEno"
             (sut/run "CONCATENATE(false, \"no\")"))))
  )

(t/deftest exact-function-test
  (t/testing "EXACT function"
    (t/is (= true
           (sut/run "EXACT(\"yes\", \"yes\" )"))))
  )

(t/deftest find-function-test
  (t/testing "FIND function"
    (let [context {:data {:name "Miriam McGovern"}}]
      (t/is (= 1
               (sut/run "FIND(\"M\", data.name)" context)))
      (t/is (= 6
               (sut/run "FIND(\"m\", data.name)" context)))
      (t/is (= 8
               (sut/run "FIND(\"M\", data.name, 3)" context)))))
  )

(t/deftest left-function-test
  (t/testing "LEFT function"
    (t/is (= "Sale"
             (sut/run "LEFT(\"Sale Price\", 4)")))
    (t/is (= "S"
             (sut/run "LEFT(\"Sweeden\")")))
    (t/is (= "Sale Price"
             (sut/run "LEFT(\"Sale Price\", 12)")))))

(t/deftest len-function-test
  (t/testing "LEN function"
    (t/is (= 4
             (sut/run "LEN(\"four\")")))
    (t/is (= 8
             (sut/run "LEN(\"four    \")")))
    (t/is (= 3
             (sut/run "LEN({\"foo\"})")))))

(t/deftest lower-function-test
  (t/testing "LOWER function"
    (t/is (= "abcd"
             (sut/run "LOWER(\"abcd\")")))
    (t/is (= "abcd"
             (sut/run "LOWER(\"ABcd\")")))
    (t/is (= "abcd"
             (sut/run "LOWER(\"ABCD\")")))
    (t/is (= ""
             (sut/run "LOWER(\"\")"))))

  ;; TODO: text.LOWER().should.equal(error.value);
  )

(t/deftest mid-function-test
  (t/testing "MID function"
    (let [context {:data "Fluid Flow"}]
      (t/is (= "Fluid"
               (sut/run "MID(data, 1, 5)" context)))
      (t/is (= "Flow"
               (sut/run "MID(data, 7, 20)" context)))
      (t/is (= ""
               (sut/run "MID(data, 20, 50)" context)))

      )))

(t/deftest proper-function-test
  (t/testing "PROPER function"
    (t/is (= "A Title Case"
             (sut/run "PROPER(\"a title case\")")))
    (t/is (= "True"
             (sut/run "PROPER(true)")))
    (t/is (= "90"
             (sut/run "PROPER(90)")))
    (t/is (= "Foo-Bar.Baz"
             (sut/run "PROPER(\"foo-bar.baz\")")))
    ))

(t/deftest replace-function-test
  (t/testing "REPLACE function"
    (t/is (= "abcde*k"
             (sut/run "REPLACE(\"abcdefghijk\", 6, 5, \"*\")")))
    (t/is (= "2010"
             (sut/run "REPLACE(\"2009\", 3, 2, \"10\")")))
    (t/is (= "@456"
             (sut/run "REPLACE(\"123456\", 1, 3, \"@\")")))

    ))

(t/deftest rept-function-test
  (t/testing "REPT function"
    (t/is (= "foo foo foo "
             (sut/run "REPT(\"foo \", 3)")))
    ))

(t/deftest right-function-test
  (t/testing "RIGHT function"
    (t/is (= "Price"
             (sut/run "RIGHT(\"Sale Price\", 5)")))
    (t/is (= "r"
             (sut/run "RIGHT(\"Stock Number\")")))
    ))

(t/deftest search-function-test
  (t/testing "SEARCH function"
    (t/is (= 7
             (sut/run "SEARCH(\"e\", \"Statements\", 6)")))
    (t/is (= 8
             (sut/run "SEARCH(\"margin\", \"Profit Margin\")")))
    (t/is (= 1
             (sut/run "SEARCH(\"ba\", \"bar\")")))
    ))

(t/deftest split-function-test
  (t/testing "SPLIT function"
    (t/is (= ["foo" "bar" "baz"]
             (sut/run "SPLIT(\"foo/bar/baz\", \"/\")")))
    ))


(t/deftest substitute-function-test
  (t/testing "SUBSTITUTE function"
    (t/is (= "James Alateras"
           (sut/run "SUBSTITUTE(\"Jim Alateras\", \"im\", \"ames\")")))
    (t/is (= "Jim Alateras"
           (sut/run "SUBSTITUTE(\"Jim Alateras\", \"\", \"ames\")")))
    (t/is (= ""
             (sut/run "SUBSTITUTE(\"\", \"im\", \"ames\")")))
    (t/is (= "Quarter 2, 2008"
             (sut/run "SUBSTITUTE(\"Quarter 1, 2008\", \"1\", \"2\", 1)")))
    (t/is (= "Jim Alateras"
             (sut/run "SUBSTITUTE(\"Jim Alateras\", \"\", \"ames\", 1)")))
    (t/is (= "Jim Alateras Jim Alateras James Alateras"
             (sut/run "SUBSTITUTE(\"Jim Alateras Jim Alateras Jim Alateras\", \"im\", \"ames\", 3)")))
    (t/is (= "James Alateras"
             (sut/run "SUBSTITUTE(\"James Alateras\", \"im\", \"ames\", 2)")))
    (t/is (= "1"
             (sut/run "SUBSTITUTE(1, \"foo\", \"bar\")")))
    ))

(t/deftest trim-function-test
  (t/testing "TRIM function"
    (t/is (= "more spaces"
             (sut/run "TRIM(\" more     spaces \")")))
    ))

(t/deftest upper-function-test
  (t/testing "UPPER function"
    (t/is (= "TO UPPER CASE PLEASE"
             (sut/run "UPPER(\"to upper case please\")")))
    (t/is (= "1"
             (sut/run "UPPER(1)")))
    ))

(t/deftest count-function-test
  (let [context {:data [1 2 3]}]
    (t/testing "COUNT function"
      (t/is (= 3
               (sut/run "COUNT(data)" context)))
      (t/is (= 4
               (sut/run "COUNT(1,2,3,4)")))
      ))
  )

(t/deftest clean-function-test
  (let [example (str "foo" (apply str (map char (range 0 35))))]
    (t/is (= "foo !\""
             (sut/run "CLEAN(foo)" {:foo example})))))

(t/deftest char-fn-test
  (t/is (= {:type "#VALUE!" :reason "Function CHAR parameter 1 expects number values. But 'd' is a text."}
           (sut/run "CHAR(\"d\")")))

  (t/is (= {:type "#NUM!"
            :reason "Function CHAR parameter 1 value 65536 is out of range."}
           (sut/run "CHAR(65536)")))

  (t/is (= "d" (sut/run "CHAR(100)"))))

(t/deftest dollar-function-test

  (t/testing "DOLLAR function should work as expected"

    (t/is (= "$100" (sut/run "DOLLAR(100, 0)")))

    (t/is (= "$100.00" (sut/run "DOLLAR(100)")))

    (t/is (= "$90" (sut/run "DOLLAR(89, -1)")))

    (t/is (= {:type "#VALUE!"
              :reason "Function DOLLAR parameter 1 expects number values."}
             (sut/run "DOLLAR(\"foo\")")))

    (t/is (= {:type "#VALUE!"
              :reason "Function DOLLAR parameter 2 expects number values."}
             (sut/run "DOLLAR(100, \"foo\")")))))

(t/deftest wrong-arity-test
  (t/is (= {:type "#N/A", :reason "Wrong number of arguments to JOIN. Expected at least 2 arguments, but got 1 arguments."}
           (sut/run "JOIN(\",\")")))

  (t/is (= {:type "#N/A"
            :reason "Wrong number of arguments to CHAR. Expected exact 1 argument, but got 0 arguments."}
           (sut/run "CHAR()"))))
