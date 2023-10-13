(ns axel-f.functions-test
  (:require [axel-f.excel :as af]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true]))
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(t/deftest operators

  (t/is (= 2
           ((af/compile "1 + 1"))))

  (t/is (= 0
           ((af/compile "1 - 1"))))

  (t/is (= 4
           ((af/compile "2 * 2"))))

  (t/is (= 2
           ((af/compile "4 / 2"))))

  (t/is (= true
           ((af/compile "1 < 2"))))
  (t/is (= false
           ((af/compile "1 < 1"))))
  (t/is (= false
           ((af/compile "2 < 1"))))

  (t/is (= false
           ((af/compile "1 > 2"))))
  (t/is (= false
           ((af/compile "1 > 1"))))
  (t/is (= true
           ((af/compile "2 > 1"))))

  (t/is (= true
           ((af/compile "1 <= 2"))))
  (t/is (= true
           ((af/compile "1 <= 1"))))
  (t/is (= false
           ((af/compile "2 <= 1"))))

  (t/is (= false
           ((af/compile "1 >= 2"))))
  (t/is (= true
           ((af/compile "1 >= 1"))))
  (t/is (= true
           ((af/compile "2 >= 1"))))

  (t/is (= "qweewq"
           ((af/compile "'qwe' & 'ewq'"))))

  (t/is (= 4.0
           ((af/compile "2 ^ 2"))))

  (t/is (= 1
           ((af/compile "--TRUE"))))

  (t/is (= 0
           ((af/compile "--False"))))

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Fail to coerce `foo` to number."
         ((af/compile "--'foo'")))))

(t/deftest operator-precedence

  (t/is (= 6
           ((af/compile "2 + 2 * 2"))))

  (t/is (= 8
           ((af/compile "(2 + 2) * 2"))))

  (t/is (= 8
           ((af/compile "2 * (2 + 2)")))))

(t/deftest references

  (t/is (= 1
           ((af/compile "foo.'foo'") {"foo" {:foo 1}}) ))

  (t/is (= 1
           ((af/compile ":foo/bar") {"foo/bar" 1})))

  (t/is (= 1
           ((af/compile "foo[bar]") {:foo [1 2 3]
                                     :bar 0})))

  (t/is (= 1
           ((af/compile "foo[:foo/bar]") {:foo [1 2 3]
                                          :foo/bar 0})))

  (t/is (= 4
           ((af/compile "foo[:foo/bar + 2]") {:foo [1 2 3 4]
                                              :foo/bar 1})))

  (t/is (= 3
           ((af/compile "foo[1 + 1]") {:foo [1 2 3]})))

  (t/is (= [1 2 3]
           ((af/compile "foo[]") {:foo [1 2 3]}))))

(t/deftest special-functions

  (t/is (= [1 2 3]
           ((af/compile "MAP(FN(x, x), {1,2,3})"))))

  (t/is (= [1 1 1]
           ((af/compile "MAP(FN(1), {1, 2, 3})"))))

  (t/is (= [2 3 4]
           ((af/compile "WITH(inc, FN(x, x + 1), MAP(inc, {1,2,3}))"))))

  (t/is (= [1 2 3 4]
           ((af/compile "CONCAT({1,2}, {3,4})"))))

  (t/is (= [1 3]
           ((af/compile "FILTER(FN(x, x <> 2), {1,2,3})"))))

  (t/is (= [1 3]
           ((af/compile "WITH(nottwo, FN(x, x <> 2), KEEP(nottwo, foo))" {:foo [1 2 3]}))))

  (t/is (= [{:foo 1} {:foo 2} {:foo 3}]
           ((af/compile "SORT(FN(x, x.foo), _)") [{:foo 3} {:foo 1} {:foo 2}])))

  (t/is (= [{:foo 1} {:foo 2} {:foo 3}]
           ((af/compile "WITH(by-foo, FN(x, x.foo), SORT(by-foo, _))") [{:foo 3} {:foo 1} {:foo 2}])))

  (t/is (= 1
           ((af/compile "IF(1 = 1, 1, 2)"))))

  (t/is (= 1
           ((af/compile "IF(1 = 1, 1)"))))

  (t/is (= nil
           ((af/compile "IF(1 = 2, 2)"))))

  (t/is (= 1
           ((af/compile "IFS(true, 1, false, 2)"))))

  (t/is (= "found two"
           ((af/compile "IFS(foo = 1, 'found one', _.foo = 2, 'found two')") {:foo 2})))

  (t/is (= 1
           ((af/compile "IFS(foo = 1, 0, 1)") {:foo 2})))

  (t/is (= nil
           ((af/compile "IFS(false, 1, false, 2)")))))

(t/deftest closures

  (t/is (= "foobar"
           ((af/compile "WITH(
                           foo, 'f' & 'o' & 'o',
                           bar, CONCATENATE('b', 'a', 'r'),
                           foo & bar
                         )"))))

  (t/is (= [1 2 3]
           ((af/compile "WITH(
                           x, 1,
                           y, x + 1,
                           z, y + 1,
                           {x, y, z}
                         )"))))

  (let [f1 (-> "CONCATENATE(MAP(inc, foo), _.bar)" (af/compile {"inc" inc}))
        f2 (-> "CONCATENATE(foo, _.bar)" (af/compile {:foo [0 1 2]}))]

    (t/is (= "123"
             (f1 {:foo [0 1 2]})))

    (t/is (= "123456"
             (f1 {:foo [0 1 2] :bar [4 5 6]})))

    (t/is (= "012"
             (f2 {:foo [1 2 3]})))

    (t/is (= "012456"
             (f2 {:foo [1 2 3] :bar [4 5 6]})))))

(t/deftest math-functions

  (t/is (= 6
           ((af/compile "SUM(1,2,3)"))))

  (t/is (= 6
           ((af/compile "SUM(_)") [1 2 3])))

  (t/is (= 12
           ((af/compile "SUM(foo.bar, 6)") {:foo {"bar" [1 2 3]}})))

  (t/are [x y] (t/is (= x ((af/compile (str "ROUND(123123.123, " y ")")))))
    123000     -3
    123100     -2
    123120     -1
    123123     0
    123123.1   1
    123123.12  2
    123123.123 3))

(t/deftest logic-functions

  (t/are [x y] (t/is (= x ((af/compile (str "OR(" y ")")) {:foo [1 2 3] :bar []})))
    true "1 > 0, 2 > 1"
    true "1 > 0, 2 < 1"
    true "COUNT(foo)"
    false "AVERAGE(foo) < COUNT(bar)")

  (t/are [x y] (t/is (= x ((af/compile (str "AND(" y ")")) {:foo [1 2 3] :bar []})))
    true "1 > 0, 2 > 1"
    false "1 > 0, 2 < 1"
    true "COUNT(foo)"
    false "AVERAGE(foo) < COUNT(bar)")

  (t/are [x y] (t/is (= x ((af/compile (str "NOT(" y ")")) {:foo [1 2 3]})))
    true "1 > 2"
    false "True"
    true "False"))

(t/deftest stat-functions

  (t/are [x y] (t/is (= x ((af/compile (str "MIN(" y ")")) {:foo [1 2 3]})))
    1 "{1}"
    1 "foo")

  (t/are [x y] (t/is (= x ((af/compile (str "MAX(" y ")")) {:foo [1 2 3]})))
    1 "{1}"
    3 "foo")

  (t/are [x y] (t/is (= x ((af/compile (str "COUNT(" y ")")) {:foo [1 2 3]})))
    0 "\"foo\""
    1 "{\"foo\", 1}"
    1 "{1}"
    3 "foo")

  (t/are [x y] (t/is (= x ((af/compile (str "AVERAGE(" y ")")) {:foo [1 2 3] :bar []})))
    2 "{1,2,3}"
    nil "bar"
    2 "foo"
    1 "1, 1, TRUE"))


(t/deftest code-function-test
  (t/testing "CODE function"
    (t/is (= 65
             ((af/compile "CODE(\"A\")"))))
    (t/is (= 1000
             ((af/compile "CODE(\"Ϩ\")"))))

    (t/is (nil? ((af/compile "CODE(\"\")"))))))

(t/deftest concatenate-1-function-test
  (t/testing "CONCATENATE function"
    (t/is (= "hello world"
             ((af/compile "CONCATENATE(\"hello\", \" \", \"world\")"))))
    (t/is (= "hello world"
             ((af/compile "CONCATENATE({\"hello\", \" \", \"world\"})"))))
    (t/is (= "1hello"
             ((af/compile "CONCATENATE(1, \"hello\",)"))))
    (t/is (= "TRUEyes"
             ((af/compile "CONCATENATE(true, \"yes\")"))))
    (t/is (= "FALSEno"
             ((af/compile "CONCATENATE(false, \"no\")"))))))

(t/deftest exact-function-test
  (t/testing "EXACT function"
    (t/is (= true
             ((af/compile "EXACT(\"yes\", \"yes\" )"))))))

(t/deftest find-function-test
  (t/testing "FIND function"
    (let [context {:data {:name "Miriam McGovern"}}]
      (t/is (= 1
               ((af/compile "FIND(\"M\", data.name)") context)))
      (t/is (= 6
               ((af/compile "FIND(\"m\", data.name)") context)))
      (t/is (= 8
               ((af/compile "FIND(\"M\", data.name, 3)") context))))))

(t/deftest left-function-test
  (t/testing "LEFT function"
    (t/is (= "Sale"
             ((af/compile "LEFT(\"Sale Price\", 4)"))))
    (t/is (= "S"
             ((af/compile "LEFT(\"Sweeden\")"))))
    (t/is (= "Sale Price"
             ((af/compile "LEFT(\"Sale Price\", 12)"))))))

(t/deftest len-function-test
  (t/testing "LEN function"
    (t/is (= 4
             ((af/compile "LEN(\"four\")"))))
    (t/is (= 8
             ((af/compile "LEN(\"four    \")"))))
    (t/is (= 3
             ((af/compile "LEN({\"foo\"})"))))))

(t/deftest lower-function-test
  (t/testing "LOWER function"
    (t/is (= "abcd"
             ((af/compile "LOWER(\"abcd\")"))))
    (t/is (= "abcd"
             ((af/compile "LOWER(\"ABcd\")"))))
    (t/is (= "abcd"
             ((af/compile "LOWER(\"ABCD\")"))))
    (t/is (= ""
             ((af/compile "LOWER(\"\")"))))))

(t/deftest mid-function-test
  (t/testing "MID function"
    (let [context {:data "Fluid Flow"}]
      (t/is (= "Fluid"
               ((af/compile "MID(data, 1, 5)") context)))

      (t/is (= "Flow"
               ((af/compile "MID(data, 7, 20)") context)))

      (t/is (= ""
               ((af/compile "MID(data, 20, 50)") context))))))

(t/deftest proper-function-test
  (t/testing "PROPER function"
    (t/is (= "A Title Case"
             ((af/compile "PROPER(\"a title case\")"))))

    (t/is (= "True"
             ((af/compile "PROPER(true)"))))

    (t/is (= "90"
             ((af/compile "PROPER(90)"))))

    (t/is (= "Foo-Bar.Baz"
             ((af/compile "PROPER(\"foo-bar.baz\")"))))))

(t/deftest regexextract-function-test
  (t/testing "REGEXEXTRACT function"
    (t/is (= "826.25"
             ((af/compile "REGEXEXTRACT('The price today is $826.25', '[0-9]*\\\\.[0-9]+[0-9]+')"))))

    (t/is (= "Content"
             ((af/compile "REGEXEXTRACT('(Content) between brackets', '\\(([A-Za-z]+)\\)')"))))

    (t/is (= nil
             ((af/compile "REGEXEXTRACT('FOO', '[a-z]+')"))))))

(t/deftest regex-function-test
  (t/testing "REGEX and REGEXEXTRACT functions"
    (t/is (= "Test"
             ((af/compile "REGEXEXTRACT('foo Test bar', REGEX('test', 'i'))"))))

    (t/is (= "Foo"
             ((af/compile "REGEXEXTRACT('X
Foo
bar', REGEX('^foo$', 'im'))"))))

    (t/is (= "Foo\n"
             ((af/compile "REGEXEXTRACT('Foo
', REGEX('.*', 's'))"))))))

(t/deftest regexmatch-function-test
  (t/testing "REGEXMATCH function"
    (t/is (= true
             ((af/compile "REGEXMATCH('The price today is $826.25', '[0-9]*\\.[0-9]+[0-9]+')"))))

    (t/is (= true
             ((af/compile "REGEXMATCH('(Content) between brackets', '\\(([A-Za-z]+)\\)')"))))

    (t/is (= false
             ((af/compile "REGEXMATCH('FOO', '[a-z]+')"))))))

(t/deftest regexreplace-function-test
  (t/testing "REGEXREPLACE function"
    (t/is (= "The price today is $0.00"
             ((af/compile "REGEXREPLACE('The price today is $826.25', '[0-9]*\\\\.[0-9]+[0-9]+', '0.00')"))))

    (t/is (= "Word between brackets"
             ((af/compile "REGEXREPLACE('(Content) between brackets', '\\\\(([A-Za-z]+)\\\\)', 'Word')"))))

    (t/is (= "FOO"
             ((af/compile "REGEXREPLACE('FOO', '[a-z]+', 'OOF')"))))))

(t/deftest replace-function-test
  (t/testing "REPLACE function"
    (t/is (= "abcde*k"
             ((af/compile "REPLACE(\"abcdefghijk\", 6, 5, \"*\")"))))

    (t/is (= "2010"
             ((af/compile "REPLACE(\"2009\", 3, 2, \"10\")"))))

    (t/is (= "@456"
             ((af/compile "REPLACE(\"123456\", 1, 3, \"@\")"))))))

(t/deftest rept-function-test
  (t/testing "REPT function"
    (t/is (= "foo foo foo "
             ((af/compile "REPT(\"foo \", 3)"))))))

(t/deftest right-function-test
  (t/testing "RIGHT function"
    (t/is (= "Price"
             ((af/compile "RIGHT(\"Sale Price\", 5)"))))

    (t/is (= "r"
             ((af/compile "RIGHT(\"Stock Number\")"))))

    (t/is (= "Price"
             ((af/compile "RIGHT(\"Price\", 10)"))))))

(t/deftest arabic-function-test
  (t/testing "ARABIC function"

    (t/are [x y] (= x ((af/compile (str "ARABIC('" y "')"))))
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

    (t/are [x y] (= y ((af/compile (str "ROMAN(" x ")"))))
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
             ((af/compile "SEARCH(\"e\", \"Statements\", 6)"))))

    (t/is (= 8
             ((af/compile "SEARCH(\"margin\", \"Profit Margin\")"))))

    (t/is (= 1
             ((af/compile "SEARCH(\"ba\", \"bar\")"))))))

    (t/is (= -1
         ((af/compile "SEARCH(\"xyz\", \"Statements\")"))))

(t/deftest split-function-test
  (t/testing "SPLIT function"
    (t/is (= ["foo" "bar" "baz"]
             ((af/compile "SPLIT(\"foo/bar/baz\", \"/\")"))))

    (t/is (= ["foo" "bar" "baz"]
             ((af/compile "SPLIT(\"foo!bar,baz\", \"!,\", TRUE)"))))

    (t/is (= ["foo" "" "baz"]
             ((af/compile "SPLIT(\"foonnbaz\", \"n\", FALSE, FALSE)"))))))

(t/deftest substitute-function-test
  (t/testing "SUBSTITUTE function"
    (t/is (= "James Alateras"
             ((af/compile "SUBSTITUTE(\"Jim Alateras\", \"im\", \"ames\")"))))
    (t/is (= "Jim Alateras"
             ((af/compile "SUBSTITUTE(\"Jim Alateras\", \"\", \"ames\")"))))
    (t/is (= ""
             ((af/compile "SUBSTITUTE(\"\", \"im\", \"ames\")"))))
    (t/is (= "Quarter 2, 2008"
             ((af/compile "SUBSTITUTE(\"Quarter 1, 2008\", \"1\", \"2\", 1)"))))
    (t/is (= "Jim Alateras"
             ((af/compile "SUBSTITUTE(\"Jim Alateras\", \"\", \"ames\", 1)"))))
    (t/is (= "Jim Alateras Jim Alateras James Alateras"
             ((af/compile "SUBSTITUTE(\"Jim Alateras Jim Alateras Jim Alateras\", \"im\", \"ames\", 3)"))))
    (t/is (= "James Alateras"
             ((af/compile "SUBSTITUTE(\"James Alateras\", \"im\", \"ames\", 2)"))))
    (t/is (= "1"
             ((af/compile "SUBSTITUTE(1, \"foo\", \"bar\")"))))))

(t/deftest t-function-text
  (t/testing "T function"

    (t/is (= "foo"
             ((af/compile "T('foo')"))))

    (t/is (nil? ((af/compile "T(123)"))))))

(t/deftest trim-function-test
  (t/testing "TRIM function"
    (t/is (= "more spaces"
             ((af/compile "TRIM(\" more     spaces \")"))))))

(t/deftest upper-function-test
  (t/testing "UPPER function"
    (t/is (= "TO UPPER CASE PLEASE"
             ((af/compile "UPPER(\"to upper case please\")"))))
    (t/is (= "1"
             ((af/compile "UPPER(1)"))))))

(t/deftest value-function-test
  (t/testing "VALUE function"

    (t/is (= 123.1
             ((af/compile "VALUE('123.1')"))))

    (t/is (= 0
             ((af/compile "VALUE('')"))))

    (t/is (= 0
             ((af/compile "VALUE(0)"))))))

(t/deftest clean-function-test
  (let [example (str "foo" (apply str (map char (range 0 35))))]
    (t/is (= "foo !\""
             ((af/compile "CLEAN(foo)") {:foo example})))))

(t/deftest char-fn-test
  (t/is (= "d" ((af/compile "CHAR(100)")))))

(t/deftest dollar-function-test

  (t/testing "DOLLAR function should work as expected"

    (t/is (= "$100"
             ((af/compile "DOLLAR(100, 0)"))))

    (t/is (= "$100.00"
             ((af/compile "DOLLAR(100)"))))

    (t/is (= "$90"
             ((af/compile "DOLLAR(89, -1)"))))))

(t/deftest join-function

  (t/is (= "fish and chips"
           ((af/compile "JOIN(' and ', MAP(FN(x, JOIN('or', x)), foo))") {:foo ["fish" "chips"]}))))

(t/deftest textjoin-function

  (t/is (= "foo, bar, baz"
           ((af/compile "TEXTJOIN(', ', TRUE, _)") ["foo" "bar" "baz"])))

  (t/is (= "foo, baz"
           ((af/compile "TEXTJOIN(\", \", TRUE, _)") ["foo" nil "baz"])))

  (t/is (= "foo, NULL, baz"
           ((af/compile "TEXTJOIN(\", \", FALSE, _)") ["foo" nil "baz"]))))

(t/deftest complex-example

  (let [f "
WITH(
  starting-point, Position.GpsDataCompressed.gpsPoint,
  normalized, WITH(
                gps-delta, Position.GpsDataCompressed.gpsDelta,
                normalize, FN(pos, WITH(
                                     normalize, FN(x, IF(x > 2^15, 2^15 - x, x) / 600000),
                                     latitude, normalize(pos.latitude),
                                     longitude, normalize(pos.longitude),
                                     OBJECT.MERGE(pos, OBJECT.NEW({{'latitude', latitude}, {'longitude', longitude}}))
                                   )),
                MAP(normalize, gps-delta)
              ),
  decompressor, FN(i, OBJECT.MERGE(normalized[i],
                                   OBJECT.NEW({{'latitude', starting-point.latitude + SUM(MAP(FN(p, p.latitude), normalized[0 : INC(i)]))},
                                               {'longitude', starting-point.longitude + SUM(MAP(FN(p, p.longitude), normalized[0 : INC(i)]))}}))),
  into-point, FN(c, {c.latitude, c.longitude}),
  decompressed, MAP(decompressor, 0 : LENGTH(normalized)),
  ROUND(GEO.DISTANCE({into-point(starting-point), into-point(decompressed[0])}) + GEO.DISTANCE(MAP(into-point, decompressed)), 10)
)
"
        f (af/compile f)
        data {"Position" {"GpsDataCompressed" {"gpsDelta" [{"latitude" 9 "longitude" 32872 "time" 120}
                                                           {"latitude" 32844 "longitude" 19 "time" 240}
                                                           {"latitude" 64 "longitude" 37 "time" 180}]
                                               "gpsPoint" {"latitude" 48.723717
                                                           "longitude" 9.1222725
                                                           "speed" 0.8791895
                                                           "heading" 0
                                                           "dateTime" 1521099587000}
                                               "neglect" 0}
                    "GpsPoint" {"latitude" 47.905552
                                "longitude" 16.274143
                                "speed" 0.62131244
                                "heading" 298.64
                                "dateTime" 1521105337000
                                "height" 269.46667}}}]
    (t/is (= 0.0397927388 (f data)))))

(comment

  (def f "
WITH(
  starting-point, Position.GpsDataCompressed.gpsPoint,
  normalized, WITH(
                gps-delta, Position.GpsDataCompressed.gpsDelta,
                normalize, FN(pos, WITH(
                                     normalize, FN(x, IF(x > 2^15, 2^15 - x, x) / 600000),
                                     latitude, normalize(pos.latitude),
                                     longitude, normalize(pos.longitude),
                                     OBJECT.MERGE(pos, OBJECT.NEW({{'latitude', latitude}, {'longitude', longitude}}))
                                   )),
                MAP(normalize, gps-delta)
              ),
  decompressor, FN(i, OBJECT.MERGE(normalized[i],
                                   OBJECT.NEW({{'latitude', starting-point.latitude + SUM(MAP(FN(p, p.latitude), normalized[0 : INC(i)]))},
                                               {'longitude', starting-point.longitude + SUM(MAP(FN(p, p.longitude), normalized[0 : INC(i)]))}}))),
  into-point, FN(c, {c.latitude, c.longitude}),
  decompressed, MAP(decompressor, 0 : LENGTH(normalized)),
  ROUND(GEO.DISTANCE({into-point(starting-point), into-point(decompressed[0])}) + GEO.DISTANCE(MAP(into-point, decompressed)), 10)
)
")

  (def data {"Position" {"GpsDataCompressed" {"gpsDelta" [{"latitude" 9 "longitude" 32872 "time" 120}
                                                          {"latitude" 32844 "longitude" 19 "time" 240}
                                                          {"latitude" 64 "longitude" 37 "time" 180}]
                                              "gpsPoint" {"latitude" 48.723717
                                                          "longitude" 9.1222725
                                                          "speed" 0.8791895
                                                          "heading" 0
                                                          "dateTime" 1521099587000}
                                              "neglect" 0}
                         "GpsPoint" {"latitude" 47.905552
                                     "longitude" 16.274143
                                     "speed" 0.62131244
                                     "heading" 298.64
                                     "dateTime" 1521105337000
                                     "height" 269.46667}}})

  (time
   (dotimes [_ 1000]
     (af/compile f)))

  (let [f (af/compile f)]
    (time
     (dotimes [_ 1000]
       (f data))))

  )
