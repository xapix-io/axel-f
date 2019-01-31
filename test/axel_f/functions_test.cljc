(ns axel-f.functions-test
  (:require [axel-f.lexer :as l]
            [axel-f.parser :as p]
            [axel-f.runtime :as r]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true]))
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(defn- eval*
  ([f]
   ((-> f l/read-formula p/parse r/eval)))
  ([f c]
   ((-> f l/read-formula p/parse r/eval) c)))

(t/deftest operators

  (t/is (= 2
           (eval* "1 + 1")))

  (t/is (= 0
           (eval* "1 - 1")))

  (t/is (= 4
           (eval* "2 * 2")))

  (t/is (= 2
           (eval* "4 / 2")))

  (t/is (= true
           (eval* "1 < 2")))
  (t/is (= false
           (eval* "1 < 1")))
  (t/is (= false
           (eval* "2 < 1")))

  (t/is (= false
           (eval* "1 > 2")))
  (t/is (= false
           (eval* "1 > 1")))
  (t/is (= true
           (eval* "2 > 1")))

  (t/is (= true
           (eval* "1 <= 2")))
  (t/is (= true
           (eval* "1 <= 1")))
  (t/is (= false
           (eval* "2 <= 1")))

  (t/is (= false
           (eval* "1 >= 2")))
  (t/is (= true
           (eval* "1 >= 1")))
  (t/is (= true
           (eval* "2 >= 1")))

  (t/is (= "qweewq"
           (eval* "'qwe' & 'ewq'")))

  (t/is (= 4.0
           (eval* "2 ^ 2")))

  (t/is (= 1
           (eval* "--TRUE")))

  (t/is (= 0
           (eval* "--False")))

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Fail to coerce .*"
         (eval* "--'foo'"))))

(t/deftest references

  (t/is (= 1
           (eval* "foo.'foo'" {"foo" {:foo 1}})))

  (t/is (= 1
           (eval* ":foo/bar" {"foo/bar" 1})))

  (t/is (= 1
           (eval* "['foo']" {:foo 1}))))

(t/deftest special-functions

  (t/is (= [1 2 3]
           (eval* "MAP(_, {1,2,3})")))

  (t/is (= [1 3]
           (eval* "FILTER(_ <> 2, {1,2,3})")))

  (t/is (= [{:foo 1} {:foo 2} {:foo 3}]
           (eval* "SORT(_.foo, _)" [{:foo 3} {:foo 1} {:foo 2}])))

  (t/is (= 1
           (eval* "IF(1 = 1, 1, 2)")))

  (t/is (= 1
           (eval* "IF(1 = 1, 1)")))

  (t/is (= nil
           (eval* "IF(1 = 2, 2)")))

  (t/is (= 1
           (eval* "IFS(true, 1, false, 2)")))

  (t/is (= "found two"
           (eval* "IFS(foo = 1, 'found one', _.foo = 2, 'found two')" {:foo 2}))))

(t/deftest math-functions

  (t/is (= 6
           (eval* "SUM(1,2,3)")))

  (t/is (= 6
           (eval* "SUM(_)" [1 2 3])))

  (t/is (= 12
           (eval* "SUM(foo.bar, 6)" {:foo {"bar" [1 2 3]}})))

  (t/are [x y] (t/is (= x (eval* (str "ROUND(123123.123, " y ")"))))
    123000     -3
    123100     -2
    123120     -1
    123123     0
    123123.1   1
    123123.12  2
    123123.123 3))

(t/deftest logic-functions

  (t/are [x y] (t/is (= x (eval* (str "OR(" y ")") {:foo [1 2 3] :bar []})))
    true "1 > 0, 2 > 1"
    true "1 > 0, 2 < 1"
    true "COUNT(foo)"
    false "AVERAGE(foo) < COUNT(bar)")

  (t/are [x y] (t/is (= x (eval* (str "AND(" y ")") {:foo [1 2 3] :bar []})))
    true "1 > 0, 2 > 1"
    false "1 > 0, 2 < 1"
    true "COUNT(foo)"
    false "AVERAGE(foo) < COUNT(bar)")

  (t/are [x y] (t/is (= x (eval* (str "NOT(" y ")") {:foo [1 2 3]})))
    true "1 > 2"
    false "True"
    true "False"))

(t/deftest stat-functions

  (t/are [x y] (t/is (= x (eval* (str "MIN(" y ")") {:foo [1 2 3]})))
    1 "{1}"
    1 "foo")

  (t/are [x y] (t/is (= x (eval* (str "MAX(" y ")") {:foo [1 2 3]})))
    1 "{1}"
    3 "foo")

  (t/are [x y] (t/is (= x (eval* (str "COUNT(" y ")") {:foo [1 2 3]})))
    0 "\"foo\""
    1 "{\"foo\", 1}"
    1 "{1}"
    3 "foo")

  (t/are [x y] (t/is (= x (eval* (str "AVERAGE(" y ")") {:foo [1 2 3] :bar []})))
    2 "{1,2,3}"
    nil "bar"
    2 "foo"
    1 "1, 1, TRUE"))


(t/deftest code-function-test
  (t/testing "CODE function"
    (t/is (= 65
             (eval* "CODE(\"A\")")))
    (t/is (= 1000
             (eval* "CODE(\"Ϩ\")")))

    (t/is (nil? (eval* "CODE(\"\")")))))

(t/deftest concatenate-1-function-test
  (t/testing "CONCATENATE function"
    (t/is (= "hello world"
             (eval* "CONCATENATE(\"hello\", \" \", \"world\")")))
    (t/is (= "hello world"
             (eval* "CONCATENATE({\"hello\", \" \", \"world\"})")))
    (t/is (= "1hello"
             (eval* "CONCATENATE(1, \"hello\",)")))
    (t/is (= "TRUEyes"
             (eval* "CONCATENATE(true, \"yes\")")))
    (t/is (= "FALSEno"
             (eval* "CONCATENATE(false, \"no\")")))))

(t/deftest exact-function-test
  (t/testing "EXACT function"
    (t/is (= true
             (eval* "EXACT(\"yes\", \"yes\" )")))))

(t/deftest find-function-test
  (t/testing "FIND function"
    (let [context {:data {:name "Miriam McGovern"}}]
      (t/is (= 1
               (eval* "FIND(\"M\", data.name)" context)))
      (t/is (= 6
               (eval* "FIND(\"m\", data.name)" context)))
      (t/is (= 8
               (eval* "FIND(\"M\", data.name, 3)" context))))))

(t/deftest left-function-test
  (t/testing "LEFT function"
    (t/is (= "Sale"
             (eval* "LEFT(\"Sale Price\", 4)")))
    (t/is (= "S"
             (eval* "LEFT(\"Sweeden\")")))
    (t/is (= "Sale Price"
             (eval* "LEFT(\"Sale Price\", 12)")))))

(t/deftest len-function-test
  (t/testing "LEN function"
    (t/is (= 4
             (eval* "LEN(\"four\")")))
    (t/is (= 8
             (eval* "LEN(\"four    \")")))
    (t/is (= 3
             (eval* "LEN({\"foo\"})")))))

(t/deftest lower-function-test
  (t/testing "LOWER function"
    (t/is (= "abcd"
             (eval* "LOWER(\"abcd\")")))
    (t/is (= "abcd"
             (eval* "LOWER(\"ABcd\")")))
    (t/is (= "abcd"
             (eval* "LOWER(\"ABCD\")")))
    (t/is (= ""
             (eval* "LOWER(\"\")")))))

(t/deftest mid-function-test
  (t/testing "MID function"
    (let [context {:data "Fluid Flow"}]
      (t/is (= "Fluid"
               (eval* "MID(data, 1, 5)" context)))

      (t/is (= "Flow"
               (eval* "MID(data, 7, 20)" context)))

      (t/is (= ""
               (eval* "MID(data, 20, 50)" context))))))

(t/deftest proper-function-test
  (t/testing "PROPER function"
    (t/is (= "A Title Case"
             (eval* "PROPER(\"a title case\")")))

    (t/is (= "True"
             (eval* "PROPER(true)")))

    (t/is (= "90"
             (eval* "PROPER(90)")))

    (t/is (= "Foo-Bar.Baz"
             (eval* "PROPER(\"foo-bar.baz\")")))))

(t/deftest regexextract-function-test
  (t/testing "REGEXEXTRACT function"
    (t/is (= "826.25"
             (eval* "REGEXEXTRACT('The price today is $826.25', '[0-9]*\\\\.[0-9]+[0-9]+')")))

    (t/is (= "Content"
             (eval* "REGEXEXTRACT('(Content) between brackets', '\\(([A-Za-z]+)\\)')")))

    (t/is (= nil
             (eval* "REGEXEXTRACT('FOO', '[a-z]+')")))))

(t/deftest regexmatch-function-test
  (t/testing "REGEXMATCH function"
    (t/is (= true
             (eval* "REGEXMATCH('The price today is $826.25', '[0-9]*\\.[0-9]+[0-9]+')")))

    (t/is (= true
             (eval* "REGEXMATCH('(Content) between brackets', '\\(([A-Za-z]+)\\)')")))

    (t/is (= false
             (eval* "REGEXMATCH('FOO', '[a-z]+')")))))

(t/deftest regexreplace-function-test
  (t/testing "REGEXREPLACE function"
    (t/is (= "The price today is $0.00"
             (eval* "REGEXREPLACE('The price today is $826.25', '[0-9]*\\\\.[0-9]+[0-9]+', '0.00')")))

    (t/is (= "Word between brackets"
             (eval* "REGEXREPLACE('(Content) between brackets', '\\\\(([A-Za-z]+)\\\\)', 'Word')")))

    (t/is (= "FOO"
             (eval* "REGEXREPLACE('FOO', '[a-z]+', 'OOF')")))))

(t/deftest replace-function-test
  (t/testing "REPLACE function"
    (t/is (= "abcde*k"
             (eval* "REPLACE(\"abcdefghijk\", 6, 5, \"*\")")))

    (t/is (= "2010"
             (eval* "REPLACE(\"2009\", 3, 2, \"10\")")))

    (t/is (= "@456"
             (eval* "REPLACE(\"123456\", 1, 3, \"@\")")))))

(t/deftest rept-function-test
  (t/testing "REPT function"
    (t/is (= "foo foo foo "
             (eval* "REPT(\"foo \", 3)")))))

(t/deftest right-function-test
  (t/testing "RIGHT function"
    (t/is (= "Price"
             (eval* "RIGHT(\"Sale Price\", 5)")))

    (t/is (= "r"
             (eval* "RIGHT(\"Stock Number\")")))

    (t/is (= "Price"
             (eval* "RIGHT(\"Price\", 10)")))))

(t/deftest arabic-function-test
  (t/testing "ARABIC function"

    (t/are [x y] (= x (eval* (str "ARABIC('" y "')")))
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

    (t/are [x y] (= y (eval* (str "ROMAN(" x ")")))
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
      3999  "MMMCMXCIX")))

(t/deftest search-function-test
  (t/testing "SEARCH function"
    (t/is (= 7
             (eval* "SEARCH(\"e\", \"Statements\", 6)")))

    (t/is (= 8
             (eval* "SEARCH(\"margin\", \"Profit Margin\")")))

    (t/is (= 1
             (eval* "SEARCH(\"ba\", \"bar\")")))))

(t/deftest split-function-test
  (t/testing "SPLIT function"
    (t/is (= ["foo" "bar" "baz"]
             (eval* "SPLIT(\"foo/bar/baz\", \"/\")")))

    (t/is (= ["foo" "bar" "baz"]
             (eval* "SPLIT(\"foo!bar,baz\", \"!,\", TRUE)")))

    (t/is (= ["foo" "" "baz"]
             (eval* "SPLIT(\"foonnbaz\", \"n\", FALSE, FALSE)")))))

(t/deftest substitute-function-test
  (t/testing "SUBSTITUTE function"
    (t/is (= "James Alateras"
             (eval* "SUBSTITUTE(\"Jim Alateras\", \"im\", \"ames\")")))
    (t/is (= "Jim Alateras"
             (eval* "SUBSTITUTE(\"Jim Alateras\", \"\", \"ames\")")))
    (t/is (= ""
             (eval* "SUBSTITUTE(\"\", \"im\", \"ames\")")))
    (t/is (= "Quarter 2, 2008"
             (eval* "SUBSTITUTE(\"Quarter 1, 2008\", \"1\", \"2\", 1)")))
    (t/is (= "Jim Alateras"
             (eval* "SUBSTITUTE(\"Jim Alateras\", \"\", \"ames\", 1)")))
    (t/is (= "Jim Alateras Jim Alateras James Alateras"
             (eval* "SUBSTITUTE(\"Jim Alateras Jim Alateras Jim Alateras\", \"im\", \"ames\", 3)")))
    (t/is (= "James Alateras"
             (eval* "SUBSTITUTE(\"James Alateras\", \"im\", \"ames\", 2)")))
    (t/is (= "1"
             (eval* "SUBSTITUTE(1, \"foo\", \"bar\")")))))

(t/deftest t-function-text
  (t/testing "T function"

    (t/is (= "foo"
             (eval* "T('foo')")))

    (t/is (nil? (eval* "T(123)")))))

(t/deftest trim-function-test
  (t/testing "TRIM function"
    (t/is (= "more spaces"
             (eval* "TRIM(\" more     spaces \")")))))

(t/deftest upper-function-test
  (t/testing "UPPER function"
    (t/is (= "TO UPPER CASE PLEASE"
             (eval* "UPPER(\"to upper case please\")")))
    (t/is (= "1"
             (eval* "UPPER(1)")))))

(t/deftest value-function-test
  (t/testing "VALUE function"

    (t/is (= 123.1
             (eval* "VALUE('123.1')")))

    (t/is (= 0
             (eval* "VALUE('')")))

    (t/is (= 0
             (eval* "VALUE(0)")))))

(t/deftest clean-function-test
  (let [example (str "foo" (apply str (map char (range 0 35))))]
    (t/is (= "foo !\""
             (eval* "CLEAN(foo)" {:foo example})))))

(t/deftest char-fn-test
  (t/is (= "d" (eval* "CHAR(100)"))))

(t/deftest dollar-function-test

  (t/testing "DOLLAR function should work as expected"

    (t/is (= "$100" (eval* "DOLLAR(100, 0)")))

    (t/is (= "$100.00" (eval* "DOLLAR(100)")))

    (t/is (= "$90" (eval* "DOLLAR(89, -1)")))))

(t/deftest join-function

  (t/is (= "fish and chips"
           (eval* "JOIN(' and ', MAP(JOIN('or', _), foo))" {:foo ["fish" "chips"]}))))

(t/deftest textjoin-function

  (t/is (= "foo, bar, baz"
           (eval* "TEXTJOIN(', ', TRUE, _)" ["foo" "bar" "baz"])))

  (t/is (= "foo, baz"
           (eval* "TEXTJOIN(\", \", TRUE, _)" ["foo" nil "baz"])))

  (t/is (= "foo, , baz"
           (eval* "TEXTJOIN(\", \", FALSE, _)" ["foo" nil "baz"]))))
