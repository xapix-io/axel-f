(ns axel-f.functions-test
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [axel-f.core :as sut]))

(t/deftest nested-function-test
  (t/testing "nested function calls"
    (t/is (= {:kind :axel-f.v2.parser/fncall,
              :f "SUM",
              :args
              [{:kind :axel-f.v2.parser/fncall,
                :f "SUM",
                :args
                [{:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/const,
                  :arg 1,
                  :begin {:line 1, :column 9},
                  :end {:line 1, :column 9}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/const,
                  :arg 2,
                  :begin {:line 1, :column 11},
                  :end {:line 1, :column 11}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/const,
                  :arg 3,
                  :begin {:line 1, :column 13},
                  :end {:line 1, :column 13}}]}
               {:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/const,
                :arg 5,
                :begin {:line 1, :column 17},
                :end {:line 1, :column 17}}]}
             (sut/compile "SUM(SUM(1,2,3), 5)")))))

(t/deftest vector-args-test
  (t/testing "vectors can be passed as arguments"

    (t/is (= {:kind :axel-f.v2.parser/fncall,
              :f "SUM",
              :args
              [{:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/vector,
                :args
                [{:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/const,
                  :arg 1,
                  :begin {:line 1, :column 6},
                  :end {:line 1, :column 6}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/const,
                  :arg 2,
                  :begin {:line 1, :column 8},
                  :end {:line 1, :column 8}}
                 {:kind :axel-f.v2.parser/fncall,
                  :f :axel-f.v2.parser/const,
                  :arg 3,
                  :begin {:line 1, :column 10},
                  :end {:line 1, :column 10}}]}
               {:kind :axel-f.v2.parser/fncall,
                :f :axel-f.v2.parser/const,
                :arg 4,
                :begin {:line 1, :column 14},
                :end {:line 1, :column 14}}]}
             (sut/compile "SUM({1,2,3}, 4)")))

    (t/is (= 10 (sut/run "SUM({1,2,3},4)"))
          (= 15 (sut/run "SUM({1,2,3}, {4,5})")))))

#_(t/deftest round-function-test

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

(t/deftest count-function-test

  (t/testing "COUNT function should work as expected"

    (t/are [x y] (t/is (= x (sut/run (str "COUNT(" y ")") {:foo [1 2 3]})))
      0 "\"foo\""
      1 "{\"foo\", 1}"
      1 "{1}"
      3 "foo")))

(t/deftest min-function-test

  (t/testing "MIN function should work as expected"

    (t/are [x y] (t/is (= x (sut/run (str "MIN(" y ")") {:foo [1 2 3]})))
      1 "{1}"
      1 "foo")

    (try
      (sut/run "MIN(\"foo\")")
      (catch Throwable e
        (let [msg (.getMessage e)
              data (ex-data e)]
          (t/is (= "Invalid argument type passed to the function `MIN`"
                   msg)))))))

(t/deftest max-function-test

  (t/testing "MAX function should work as expected"

    (t/are [x y] (t/is (= x (sut/run (str "MAX(" y ")") {:foo [1 2 3]})))
      1 "{1}"
      3 "foo")

    (try
      (sut/run "MAX(\"foo\")")
      (catch Throwable e
        (let [msg (.getMessage e)
              data (ex-data e)]
          (t/is (= "Invalid argument type passed to the function `MAX`"
                   msg)))))))

(t/deftest sum-function-test

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

    (try
      (sut/run "SUM(\"foo\")")
      (catch Throwable e
        (let [msg (.getMessage e)
              data (ex-data e)]
          (t/is (= "Invalid argument type passed to the function `SUM`"
                   msg)))))))

(t/deftest concatenate-function-test

  (t/testing "CONCATENATE function should work as expected"

    (t/are [x y] (t/is (= x (sut/run (str "CONCATENATE(" y ")") {:foo [1 2 3]})))
      "1" "{1}"
      "2" "foo[1]"
      "123" "foo")))

(t/deftest if-function-test

  (t/testing "IF function should work as expected"

    (t/are [x y] (t/is (= x (sut/run (str "IF(" y ")") {:foo [1 2 3]})))
      0 "FALSE, 1, 0"
      1 "TRUE, 1, 0"
      1 "True, 1"
      nil "False, 1")))

(t/deftest average-function-test

  (t/testing "AVERAGE function should work as expected"

    (t/are [x y] (t/is (= x (sut/run (str "AVERAGE(" y ")") {:foo [1 2 3] :bar []})))
      2 "{1,2,3}"
      nil "bar"
      2 "foo"
      1 "1, 1, TRUE")))

(t/deftest and-function-test

  (t/testing "AND function should work as expected"

    (t/are [x y] (t/is (= x (sut/run (str "AND(" y ")") {:foo [1 2 3] :bar []})))
      true "1 > 0, 2 > 1"
      false "1 > 0, 2 < 1"
      true "COUNT(foo)"
      false "AVERAGE(foo) < COUNT(bar)")))

(t/deftest or-function-test

  (t/testing "OR function should work as expected"

    (t/are [x y] (t/is (= x (sut/run (str "OR(" y ")") {:foo [1 2 3] :bar []})))
      true "1 > 0, 2 > 1"
      true "1 > 0, 2 < 1"
      true "COUNT(foo)"
      false "AVERAGE(foo) < COUNT(bar)")))

(t/deftest not-function-test

  (t/testing "NOT function should work as expected"

    (t/are [x y] (t/is (= x (sut/run (str "NOT(" y ")") {:foo [1 2 3]})))
      true "1 > 2"
      false "True"
      true "False")))

#_(t/deftest objref-function-test

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
             (sut/run "CODE(\"Ϩ\")")))

    (t/is (nil? (sut/run "CODE(\"\")")))))

(t/deftest concatenate-1-function-test
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

      #_(t/is (= {:type "#VALUE!"
                :reason "Function MID parameter 2 expects number values. But 'foo' is a text and cannot be coerced to a number."}
               (sut/run "MID(data, \"foo\", 0)")))

      #_(t/is (= {:type "#VALUE!"
                :reason
                "Function MID parameter 3 expects number values. But 'foo' is a text and cannot be coerced to a number."}
               (sut/run "MID(data, 2, \"foo\")"))))))

(t/deftest proper-function-test
  (t/testing "PROPER function"
    (t/is (= "A Title Case"
             (sut/run "PROPER(\"a title case\")")))

    (t/is (= "True"
             (sut/run "PROPER(true)")))

    (t/is (= "90"
             (sut/run "PROPER(90)")))

    (t/is (= "Foo-Bar.Baz"
             (sut/run "PROPER(\"foo-bar.baz\")")))))

(t/deftest regexextract-function-test
  (t/testing "REGEXEXTRACT function"
    (t/is (= "826.25"
             (sut/run "REGEXEXTRACT('The price today is $826.25', '[0-9]*\\\\.[0-9]+[0-9]+')")))

    (t/is (= "Content"
             (sut/run "REGEXEXTRACT('(Content) between brackets', '\\(([A-Za-z]+)\\)')")))

    (t/is (= nil
             (sut/run "REGEXEXTRACT('FOO', '[a-z]+')")))

    #_(t/is (= {:type "#VALUE!"
              :reason "Function REGEXEXTRACT parameter 1 expects text values. But '123' is a number and cannot be coerced to a string."}
             (sut/run "REGEXEXTRACT(123, '123')")))

    #_(t/is (= {:type "#VALUE!"
              :reason "Function REGEXEXTRACT parameter 2 expects text values. But '123' is a number and cannot be coerced to a string."}
             (sut/run "REGEXEXTRACT('123', 123)")))))

(t/deftest regexmatch-function-test
  (t/testing "REGEXMATCH function"
    (t/is (= true
             (sut/run "REGEXMATCH('The price today is $826.25', '[0-9]*\\.[0-9]+[0-9]+')")))

    (t/is (= true
             (sut/run "REGEXMATCH('(Content) between brackets', '\\(([A-Za-z]+)\\)')")))

    (t/is (= false
             (sut/run "REGEXMATCH('FOO', '[a-z]+')")))

    #_(t/is (= {:type "#VALUE!"
              :reason "Function REGEXMATCH parameter 1 expects text values. But '123' is a number and cannot be coerced to a string."}
             (sut/run "REGEXMATCH(123, '123')")))

    #_(t/is (= {:type "#VALUE!"
              :reason "Function REGEXMATCH parameter 2 expects text values. But '123' is a number and cannot be coerced to a string."}
             (sut/run "REGEXMATCH('123', 123)")))))

(t/deftest regexreplace-function-test
  (t/testing "REGEXREPLACE function"
    (t/is (= "The price today is $0.00"
             (sut/run "REGEXREPLACE('The price today is $826.25', '[0-9]*\\\\.[0-9]+[0-9]+', '0.00')")))

    (t/is (= "Word between brackets"
             (sut/run "REGEXREPLACE('(Content) between brackets', '\\\\(([A-Za-z]+)\\\\)', 'Word')")))

    (t/is (= "FOO"
             (sut/run "REGEXREPLACE('FOO', '[a-z]+', 'OOF')")))

    #_(t/is (= {:type "#VALUE!"
              :reason "Function REGEXREPLACE parameter 1 expects text values. But '123' is a number and cannot be coerced to a string."}
             (sut/run "REGEXREPLACE(123, '123', '321')")))

    #_(t/is (= {:type "#VALUE!"
              :reason "Function REGEXREPLACE parameter 2 expects text values. But '123' is a number and cannot be coerced to a string."}
             (sut/run "REGEXREPLACE('123', 123, '321')")))

    #_(t/is (= {:type "#VALUE!"
              :reason "Function REGEXREPLACE parameter 3 expects text values. But '321' is a number and cannot be coerced to a string."}
             (sut/run "REGEXREPLACE('123', '123', 321)")))))

(t/deftest replace-function-test
  (t/testing "REPLACE function"
    (t/is (= "abcde*k"
             (sut/run "REPLACE(\"abcdefghijk\", 6, 5, \"*\")")))

    (t/is (= "2010"
             (sut/run "REPLACE(\"2009\", 3, 2, \"10\")")))

    (t/is (= "@456"
             (sut/run "REPLACE(\"123456\", 1, 3, \"@\")")))

    #_(t/is (= {:type "#VALUE!"
              :reason
              "Function REPLACE parameter 2 expects number values. But 'foo' is a text and cannot be coerced to a number."}
             (sut/run "REPLACE(\"123123\", \"foo\", 5, \"*\")")))

    #_(t/is (= {:type "#VALUE!"
              :reason
              "Function REPLACE parameter 3 expects number values. But 'foo' is a text and cannot be coerced to a number."}
             (sut/run "REPLACE(\"123123\", 1, \"foo\", \"*\")")))))

(t/deftest rept-function-test
  (t/testing "REPT function"
    (t/is (= "foo foo foo "
             (sut/run "REPT(\"foo \", 3)")))

    #_(t/is (= {:type "#VALUE!"
              :reason
              "Function REPT parameter 2 expects number values. But 'bar' is a text and cannot be coerced to a number."}
             (sut/run "REPT(\"foo\", \"bar\")")))))

(t/deftest right-function-test
  (t/testing "RIGHT function"
    (t/is (= "Price"
             (sut/run "RIGHT(\"Sale Price\", 5)")))

    (t/is (= "r"
             (sut/run "RIGHT(\"Stock Number\")")))

    (t/is (= "Price"
             (sut/run "RIGHT(\"Price\", 10)")))))

(t/deftest arabic-function-test
  (t/testing "ARABIC function"

    (t/are [x y] (= x (sut/run (str "ARABIC('" y "')")))
      0     ""
      5     "V"
      9     "IX"
      12    "XII"
      16    "XVI"
      29    "XXIX"
      44    "XLIV"
      45    "XLV"
      68    "LXVIII"
      83    "LXXXIII"
      97    "XCVII"
      99    "XCIX"
      500   "D"
      501   "DI"
      649   "DCXLIX"
      798   "DCCXCVIII"
      891   "DCCCXCI"
      1000  "M"
      1004  "MIV"
      1006  "MVI"
      1023  "MXXIII"
      2014  "MMXIV"
      3999  "MMMCMXCIX"
      -5    "-V"
      -9    "-IX"
      -12   "-XII"
      -16   "-XVI"
      -29   "-XXIX"
      -44   "-XLIV"
      -45   "-XLV"
      -68   "-LXVIII"
      -83   "-LXXXIII"
      -97   "-XCVII"
      -99   "-XCIX"
      -500  "-D"
      -501  "-DI"
      -649  "-DCXLIX"
      -798  "-DCCXCVIII"
      -891  "-DCCCXCI"
      -1000 "-M"
      -1004 "-MIV"
      -1006 "-MVI"
      -1023 "-MXXIII"
      -2014 "-MMXIV"
      -3999 "-MMMCMXCIX")))

(t/deftest roman-function-test
  (t/testing "ROMAN function"

    (t/are [x y] (= y (sut/run (str "ROMAN(" x ")")))
      5     "V"
      9     "IX"
      12    "XII"
      16    "XVI"
      29    "XXIX"
      44    "XLIV"
      45    "XLV"
      68    "LXVIII"
      83    "LXXXIII"
      97    "XCVII"
      99    "XCIX"
      500   "D"
      501   "DI"
      649   "DCXLIX"
      798   "DCCXCVIII"
      891   "DCCCXCI"
      1000  "M"
      1004  "MIV"
      1006  "MVI"
      1023  "MXXIII"
      2014  "MMXIV"
      3999  "MMMCMXCIX")

    #_(t/is (= {:type "#VALUE!"
              :reason
              "Function ROMAN parameter 1 value is -1. Valid values are between 1 and 3999 inclusive."}
             (sut/run "ROMAN(-1)")))

    #_(t/is (= {:type "#VALUE!"
              :reason
              "Function ROMAN parameter 1 value is 4000. Valid values are between 1 and 3999 inclusive."}
             (sut/run "ROMAN(4000)")))

    #_(t/is (= {:type "#VALUE!"
              :reason "Function ROMAN parameter 1 expects number values. But 'foo' is a text and cannot be coerced to a number."}
             (sut/run "ROMAN(\"foo\")")))))

(t/deftest search-function-test
  (t/testing "SEARCH function"
    (t/is (= 7
             (sut/run "SEARCH(\"e\", \"Statements\", 6)")))

    (t/is (= 8
             (sut/run "SEARCH(\"margin\", \"Profit Margin\")")))

    (t/is (= 1
             (sut/run "SEARCH(\"ba\", \"bar\")")))))

(t/deftest split-function-test
  (t/testing "SPLIT function"
    (t/is (= ["foo" "bar" "baz"]
             (sut/run "SPLIT(\"foo/bar/baz\", \"/\")")))

    (t/is (= ["foo" "bar" "baz"]
             (sut/run "SPLIT(\"foo!bar,baz\", \"!,\", TRUE)")))

    (t/is (= ["foo" "" "baz"]
             (sut/run "SPLIT(\"foonnbaz\", \"n\", FALSE, FALSE)")))))

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

    #_(t/is (= {:type "#VALUE!"
              :reason "Function SUBSTITUTE parameter 4 expects number values. But 'baz' is a text and cannot be coerced to a number."}
             (sut/run "SUBSTITUTE(1, \"foo\", \"bar\", \"baz\")")))))

(t/deftest t-function-text
  (t/testing "T function"

    (t/is (= "foo"
             (sut/run "T('foo')")))

    (t/is (nil? (sut/run "T(123)")))))

(t/deftest trim-function-test
  (t/testing "TRIM function"
    (t/is (= "more spaces"
             (sut/run "TRIM(\" more     spaces \")")))))

(t/deftest upper-function-test
  (t/testing "UPPER function"
    (t/is (= "TO UPPER CASE PLEASE"
             (sut/run "UPPER(\"to upper case please\")")))
    (t/is (= "1"
             (sut/run "UPPER(1)")))))

(t/deftest value-function-test
  (t/testing "VALUE function"

    (t/is (= 123.1
             (sut/run "VALUE('123.1')")))

    (t/is (= 0
             (sut/run "VALUE('')")))

    (t/is (= 0
             (sut/run "VALUE(0)")))

    #_(t/is (= {:type "#VALUE!"
              :reason "VALUE parameter 'TRUE' cannot be parsed to number."}
             (sut/run "VALUE(TRUE)")))))

(t/deftest count-function-test-1
  (let [context {:data [1 2 3]}]
    (t/testing "COUNT function"
      (t/is (= 3
               (sut/run "COUNT(data)" context)))
      (t/is (= 4
               (sut/run "COUNT(1,2,3,4)"))))))

(t/deftest clean-function-test
  (let [example (str "foo" (apply str (map char (range 0 35))))]
    (t/is (= "foo !\""
             (sut/run "CLEAN(foo)" {:foo example})))))

(t/deftest char-fn-test
  #_(t/is (= {:type "#VALUE!" :reason "Function CHAR parameter 1 expects number values. But 'd' is a text."}
           (sut/run "CHAR(\"d\")")))

  #_(t/is (= {:type "#NUM!"
            :reason "Function CHAR parameter 1 value 65536 is out of range."}
           (sut/run "CHAR(65536)")))

  (t/is (= "d" (sut/run "CHAR(100)"))))

(t/deftest dollar-function-test

  (t/testing "DOLLAR function should work as expected"

    (t/is (= "$100" (sut/run "DOLLAR(100, 0)")))

    (t/is (= "$100.00" (sut/run "DOLLAR(100)")))

    (t/is (= "$90" (sut/run "DOLLAR(89, -1)")))

    #_(t/is (= {:type "#VALUE!"
              :reason "Function DOLLAR parameter 1 expects number values."}
             (sut/run "DOLLAR(\"foo\")")))

    #_(t/is (= {:type "#VALUE!"
              :reason "Function DOLLAR parameter 2 expects number values."}
             (sut/run "DOLLAR(100, \"foo\")")))))

(t/deftest wrong-arity-test
  #_(t/is (= {:type "#N/A", :reason "Wrong number of args (1) passed to: JOIN"}
           (sut/run "JOIN(\",\")")))

  #_(t/is (= {:type "#N/A"
            :reason "Wrong number of args (0) passed to: CHAR"}
           (sut/run "CHAR()"))))

(t/deftest map-function-test

  (t/testing "MAP function should work as expected"

    (t/is (= "Hello!World!"
             (sut/run "CONCATENATE(MAP(CONCATENATE(_, '!'), foo))" {:foo ["Hello" "World"]})))

    (t/is (= 10
             (sut/run "SUM(MAP(VALUE(_), foo))" {:foo [1 "5" 4]})))

    (t/is (= "fish and chips"
             (sut/run "JOIN(' and ', MAP(JOIN('or', _), foo))" {:foo ["fish" "chips"]})))

    (t/is (= [["[\"red!\" \"green!\" \"blue!\"] test "]
              ["[\"foo!\" \"oof!\"] test " "[\"blue!\" \"green!\" \"red!\"] test "]]
             (sut/run "MAP(MAP(MAP(_ & '!', _) & ' test ', _), foo.bar)"
               {:foo {:bar [[["red" "green" "blue"]] [["foo" "oof"] ["blue" "green" "red"]]]}})))))

(t/deftest filter-function-test

  (t/testing "FILTER function should work as expected"

    (t/is (= ["qwe" "ewqq"]
             (sut/run "FILTER(LEN(_) > 2, foo)" {:foo ["qwe" "ewqq" "1"]})))))

(t/deftest sort-function-test

  (t/testing "SORT function should work as expected"

    (t/is (= [0 123 999 90000]
             (sut/run "SORT(_, foo)" {:foo [999 123 0 90000]})))))

#_(t/deftest unique-function-test

  (t/testing "UNIQUE function should work as expected"

    (t/is (= ["qwe" 1 4321]
             (sut/run "UNIQUE(foo)" {:foo ["qwe" 1 "qwe" 4321]})))))
