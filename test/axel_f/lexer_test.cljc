(ns axel-f.lexer-test
  (:require [axel-f.lexer :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true]))
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(t/deftest read-formula

  (t/testing "end of string"

    (t/is (= [#::sut{:type ::sut/eoi
                     :position #::sut{:line 1 :column 1}}]
             (sut/read-formula ""))))

  (t/testing "whitespaces"

    (t/is (= [#::sut{:type ::sut/whitespace,
                     :value " ",
                     :begin #::sut{:line 1, :column 1},
                     :end #::sut{:line 1, :column 2},
                     :depth 0}
              #::sut{:type ::sut/eoi,
                     :position #::sut{:line 1, :column 2}}]
             (sut/read-formula " ")))

    (t/is (= [#::sut{:type ::sut/whitespace,
                     :value "    ",
                     :begin #::sut{:line 1, :column 1},
                     :end #::sut{:line 1, :column 5},
                     :depth 0}
              #::sut{:type ::sut/eoi,
                     :position #::sut{:line 1, :column 5}}]
             (sut/read-formula "    ")))

    (t/is (= [#::sut{:type ::sut/whitespace,
                     :value "    ",
                     :begin #::sut{:line 1, :column 1},
                     :end #::sut{:line 1, :column 5},
                     :depth 0}
              #::sut{:value "qwe",
                     :type ::sut/symbol,
                     :begin #::sut{:line 1, :column 5},
                     :end #::sut{:line 1, :column 7},
                     :depth 0}
              #::sut{:type ::sut/eoi,
                     :position #::sut{:line 1, :column 8}}]
             (sut/read-formula "    qwe"))))

  (t/testing "newlines"

    (t/is (= [#::sut{:type ::sut/newline,
                     :begin #::sut{:line 1, :column 1},
                     :end #::sut{:line 2, :column 1},
                     :depth 0}
              #::sut{:type ::sut/eoi,
                     :position #::sut{:line 2, :column 1}}]
             (sut/read-formula "\n")))

    (t/is (= [#::sut{:type ::sut/newline,
                     :begin #::sut{:line 1, :column 1},
                     :end #::sut{:line 5, :column 1},
                     :depth 0}
              #::sut{:type ::sut/eoi,
                     :position #::sut{:line 5, :column 1}}]
             (sut/read-formula "\n\n\n\n")))

    (t/is (= [#::sut{:type ::sut/newline,
                     :begin #::sut{:line 1, :column 1},
                     :end #::sut{:line 5, :column 1},
                     :depth 0}
              #::sut{:value "qwe",
                     :type ::sut/symbol,
                     :begin #::sut{:line 5, :column 1},
                     :end #::sut{:line 5, :column 3},
                     :depth 0}
              #::sut{:type ::sut/eoi,
                     :position #::sut{:line 5, :column 4}}]
             (sut/read-formula "\n\n\n\nqwe"))))

  (t/testing "text literals"

    (t/is (= [#::sut{:value "qwe",
                     :type ::sut/text,
                     :begin #::sut{:line 1, :column 1},
                     :end #::sut{:line 1, :column 5},
                     :depth 0}
              #::sut{:type ::sut/eoi,
                     :position #::sut{:line 1, :column 6}}]
             (sut/read-formula "\"qwe\"")
             (sut/read-formula "'qwe'")))

    (t/is (= [#::sut{:value "q\nw",
                     :type ::sut/text,
                     :begin #::sut{:line 1, :column 1},
                     :end #::sut{:line 2, :column 2},
                     :depth 0}
              #::sut{:type ::sut/eoi,
                     :position #::sut{:line 2, :column 3}}]
             (sut/read-formula "\"q\nw\"")))

    (t/is (= [#::sut{:value "qwe\n\newq\nqwe",
                     :type ::sut/text,
                     :begin #::sut{:line 1, :column 1},
                     :end #::sut{:line 4, :column 4},
                     :depth 0}
              #::sut{:value "qwe",
                     :type ::sut/symbol,
                     :begin #::sut{:line 4, :column 5},
                     :end #::sut{:line 4, :column 7},
                     :depth 0}
              #::sut{:type ::sut/eoi,
                     :position #::sut{:line 4, :column 8}}]
             (sut/read-formula "\"qwe\n\newq\nqwe\"qwe"))))

  (t/testing "number literals"

    (t/is (= [#::sut{:value 1,
                     :type ::sut/number,
                     :begin #::sut{:line 1, :column 1},
                     :end #::sut{:line 1, :column 1},
                     :depth 0}
              #::sut{:value 1.2,
                     :type ::sut/number,
                     :begin #::sut{:line 1, :column 3},
                     :end #::sut{:line 1, :column 5},
                     :depth 0}
              #::sut{:value 12.0,
                     :type ::sut/number,
                     :begin #::sut{:line 1, :column 7},
                     :end #::sut{:line 1, :column 11},
                     :depth 0}
              #::sut{:value 0.1,
                     :type ::sut/number,
                     :begin #::sut{:line 1, :column 13},
                     :end #::sut{:line 1, :column 16},
                     :depth 0}
              #::sut{:value 1.0E123,
                     :type ::sut/number,
                     :begin #::sut{:line 1, :column 18},
                     :end #::sut{:line 1, :column 25},
                     :depth 0}
              #::sut{:type ::sut/eoi,
                     :position #::sut{:line 1, :column 26}}]
             (remove sut/whitespace? (sut/read-formula "1 1.2 1.2e1 1e-1 1.0E+123")))))

  (t/testing "punctuation"

    (t/is (= [#::sut{:value ".",
                     :type ::sut/punctuation,
                     :begin #::sut{:line 1, :column 1},
                     :end #::sut{:line 1, :column 1},
                     :depth 0}
              #::sut{:value ",",
                     :type ::sut/punctuation,
                     :begin #::sut{:line 1, :column 2},
                     :end #::sut{:line 1, :column 2},
                     :depth 0}
              #::sut{:type ::sut/eoi,
                     :position #::sut{:line 1, :column 3}}]
             (sut/read-formula ".,"))))

  (t/testing "brackets"

    (t/is (= [#::sut{:value "(",
                     :type ::sut/bracket,
                     :begin #::sut{:line 1, :column 1},
                     :end #::sut{:line 1, :column 1},
                     :depth 0}
              #::sut{:value "[",
                     :type ::sut/bracket,
                     :begin #::sut{:line 1, :column 2},
                     :end #::sut{:line 1, :column 2},
                     :depth 1}
              #::sut{:value "{",
                     :type ::sut/bracket,
                     :begin #::sut{:line 1, :column 3},
                     :end #::sut{:line 1, :column 3},
                     :depth 2}
              #::sut{:value "}",
                     :type ::sut/bracket,
                     :begin #::sut{:line 1, :column 4},
                     :end #::sut{:line 1, :column 4},
                     :depth 2}
              #::sut{:value "]",
                     :type ::sut/bracket,
                     :begin #::sut{:line 1, :column 5},
                     :end #::sut{:line 1, :column 5},
                     :depth 1}
              #::sut{:value ")",
                     :type ::sut/bracket,
                     :begin #::sut{:line 1, :column 6},
                     :end #::sut{:line 1, :column 6},
                     :depth 0}
              #::sut{:type ::sut/eoi,
                     :position #::sut{:line 1, :column 7}}]
             (sut/read-formula "([{}])")))

    (t/are [x] (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                                    :cljs ExceptionInfo)
                                 #"Unbalanced brackets"
                                 (sut/read-formula x))
      "("
      "["
      "{")

    (t/are [x] (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                                    :cljs ExceptionInfo)
                                 #"Unexpected closing bracket"
                                 (sut/read-formula x))
      "]"
      "}"
      ")"))

  (t/testing "operators"

    (t/is (= [#::sut{:value ":",
                     :type ::sut/operator,
                     :begin #::sut{:line 1, :column 1},
                     :end #::sut{:line 1, :column 1},
                     :depth 0}
              #::sut{:value "+",
                     :type ::sut/operator,
                     :begin #::sut{:line 1, :column 2},
                     :end #::sut{:line 1, :column 2},
                     :depth 0}
              #::sut{:value "-",
                     :type ::sut/operator,
                     :begin #::sut{:line 1, :column 3},
                     :end #::sut{:line 1, :column 3},
                     :depth 0}
              #::sut{:value "!",
                     :type ::sut/operator,
                     :begin #::sut{:line 1, :column 4},
                     :end #::sut{:line 1, :column 4},
                     :depth 0}
              #::sut{:value ".",
                     :type ::sut/punctuation,
                     :begin #::sut{:line 1, :column 5},
                     :end #::sut{:line 1, :column 5},
                     :depth 0}
              #::sut{:value "*",
                     :type ::sut/operator,
                     :begin #::sut{:line 1, :column 6},
                     :end #::sut{:line 1, :column 6},
                     :depth 0}
              #::sut{:value "/",
                     :type ::sut/operator,
                     :begin #::sut{:line 1, :column 7},
                     :end #::sut{:line 1, :column 7},
                     :depth 0}
              #::sut{:value "&",
                     :type ::sut/operator,
                     :begin #::sut{:line 1, :column 8},
                     :end #::sut{:line 1, :column 8},
                     :depth 0}
              #::sut{:value "=",
                     :type ::sut/operator,
                     :begin #::sut{:line 1, :column 9},
                     :end #::sut{:line 1, :column 9},
                     :depth 0}
              #::sut{:value "^",
                     :type ::sut/operator,
                     :begin #::sut{:line 1, :column 10},
                     :end #::sut{:line 1, :column 10},
                     :depth 0}
              #::sut{:value "%",
                     :type ::sut/operator,
                     :begin #::sut{:line 1, :column 11},
                     :end #::sut{:line 1, :column 11},
                     :depth 0}
              #::sut{:value "<",
                     :type ::sut/operator,
                     :begin #::sut{:line 1, :column 12},
                     :end #::sut{:line 1, :column 12},
                     :depth 0}
              #::sut{:value "<=",
                     :type ::sut/operator,
                     :begin #::sut{:line 1, :column 13},
                     :end #::sut{:line 1, :column 14},
                     :depth 0}
              #::sut{:value ">",
                     :type ::sut/operator,
                     :begin #::sut{:line 1, :column 15},
                     :end #::sut{:line 1, :column 15},
                     :depth 0}
              #::sut{:value ">=",
                     :type ::sut/operator,
                     :begin #::sut{:line 1, :column 16},
                     :end #::sut{:line 1, :column 17},
                     :depth 0}
              #::sut{:value "<>",
                     :type ::sut/operator,
                     :begin #::sut{:line 1, :column 18},
                     :end #::sut{:line 1, :column 19},
                     :depth 0}
              #::sut{:type ::sut/eoi,
                     :position #::sut{:line 1, :column 20}}]
             (sut/read-formula ":+-!.*/&=^%<<=>>=<>")))))

(t/deftest expression-reading

  (t/is (= [#::sut{:value 1,
                   :type ::sut/number,
                   :begin #::sut{:line 1, :column 1},
                   :end #::sut{:line 1, :column 1},
                   :depth 0}
            #::sut{:type ::sut/whitespace,
                   :value " ",
                   :begin #::sut{:line 1, :column 2},
                   :end #::sut{:line 1, :column 3},
                   :depth 0}
            #::sut{:value "+",
                   :type ::sut/operator,
                   :begin #::sut{:line 1, :column 3},
                   :end #::sut{:line 1, :column 3},
                   :depth 0}
            #::sut{:type ::sut/whitespace,
                   :value " ",
                   :begin #::sut{:line 1, :column 4},
                   :end #::sut{:line 1, :column 5},
                   :depth 0}
            #::sut{:value 1,
                   :type ::sut/number,
                   :begin #::sut{:line 1, :column 5},
                   :end #::sut{:line 1, :column 5},
                   :depth 0}
            #::sut{:type ::sut/eoi,
                   :position #::sut{:line 1, :column 6}}]
           (sut/read-formula "1 + 1")))

  (t/is (= [#::sut{:value "!",
                   :type ::sut/operator,
                   :begin #::sut{:line 1, :column 1},
                   :end #::sut{:line 1, :column 1},
                   :depth 0}
            #::sut{:value "foo",
                   :type ::sut/symbol,
                   :begin #::sut{:line 1, :column 2},
                   :end #::sut{:line 1, :column 4},
                   :depth 0}
            #::sut{:value ".",
                   :type ::sut/punctuation,
                   :begin #::sut{:line 1, :column 5},
                   :end #::sut{:line 1, :column 5},
                   :depth 0}
            #::sut{:value "baz",
                   :type ::sut/symbol,
                   :begin #::sut{:line 1, :column 6},
                   :end #::sut{:line 1, :column 8},
                   :depth 0}
            #::sut{:value "[",
                   :type ::sut/bracket,
                   :begin #::sut{:line 1, :column 9},
                   :end #::sut{:line 1, :column 9},
                   :depth 0}
            #::sut{:value "*",
                   :type ::sut/operator,
                   :begin #::sut{:line 1, :column 10},
                   :end #::sut{:line 1, :column 10},
                   :depth 1}
            #::sut{:value "]",
                   :type ::sut/bracket,
                   :begin #::sut{:line 1, :column 11},
                   :end #::sut{:line 1, :column 11},
                   :depth 0}
            #::sut{:type ::sut/whitespace,
                   :value " ",
                   :begin #::sut{:line 1, :column 12},
                   :end #::sut{:line 1, :column 13},
                   :depth 0}
            #::sut{:value "<>",
                   :type ::sut/operator,
                   :begin #::sut{:line 1, :column 13},
                   :end #::sut{:line 1, :column 14},
                   :depth 0}
            #::sut{:type ::sut/whitespace,
                   :value " ",
                   :begin #::sut{:line 1, :column 15},
                   :end #::sut{:line 1, :column 16},
                   :depth 0}
            #::sut{:value "TRUE",
                   :type ::sut/symbol,
                   :begin #::sut{:line 1, :column 16},
                   :end #::sut{:line 1, :column 19},
                   :depth 0}
            #::sut{:type ::sut/eoi,
                   :position #::sut{:line 1, :column 20}}]
           (sut/read-formula "!foo.baz[*] <> TRUE"))))

(t/deftest errors

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Unbalanced brackets"
         (sut/read-formula "(1 + 2")))

  (try
    (sut/read-formula "(1 + 2")
    (catch ExceptionInfo e
      (let [data (ex-data e)]
        (t/is (= {:position
                  {:begin #::sut{:line 1 :column 1}
                   :end #::sut{:line 1 :column 7}}}
                 data)))))

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Unexpected closing bracket"
         (sut/read-formula "1 + 2)")))

  (try
    (sut/read-formula "1 + 2)")
    (catch ExceptionInfo e
      (let [data (ex-data e)]
        (t/is (= {:position
                  {:begin #::sut{:line 1, :column 6}
                   :end #::sut{:line 1, :column 6}}}
                 data)))))

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Unbalanced brackets"
         (sut/read-formula "(1 + 2}")))

  (try
    (sut/read-formula "(1 + 2}")
    (catch ExceptionInfo e
      (let [data (ex-data e)]
        (t/is (= {:position
                  {:begin #::sut{:line 1, :column 7}
                   :end #::sut{:line 1, :column 8}}}
                 data))))))
