(ns axel-f.parser-test
  #?@
   (:clj
    [(:require
      [axel-f.lexer :as l]
      [axel-f.parser :as sut]
      [axel-f.runtime :as r]
      [clojure.test :as t])
     (:import clojure.lang.ExceptionInfo)]
    :cljs
    [(:require
      [axel-f.lexer :as l]
      [axel-f.parser :as sut]
      [axel-f.runtime :as r]
      [cljs.test :as t :include-macros true])]))

(defn- parse*
  ([f]
   ((-> f l/read-formula sut/parse r/eval)))
  ([f c]
   ((-> f l/read-formula sut/parse r/eval) c)))

(t/deftest parse

  (t/testing "parsing constant formulas"

    (t/is (= 1
             (parse* "1")))

    (t/is (= 1.0
             (parse* "1.0")))

    (t/is (= 1.234
             (parse* "1.234")))

    (t/is (= 1.234e2
             (parse* "1.234e2")))

    (t/is (= 1.234e-2
             (parse* "1.234e-2")))

    (t/is (= 1.234e2
             (parse* "1.234e+2")))

    (t/is (= 1.234e-2
             (parse* "1.234E-2")))

    (t/is (= 1.234e2
             (parse* "1.234E2")))

    (t/is (= 1.234e2
             (parse* "1.234E+2")))

    (t/is (= "foo"
             (parse* "'foo'")))

    (t/is (= "foo"
             (parse* "\"foo\"")))

    (t/is (= "foo\n"
             (parse* "'foo\n'")))

    (t/is (= "foo+"
             (parse* "\"foo\\+\"")))

    (t/is (= "'foo'"
             (parse* "\"'foo'\"")))

    (t/is (= "\"foo\""
             (parse* "'\"foo\"'")))

    (t/testing "not properly enclosed string"
      (t/is (thrown-with-msg?
             ExceptionInfo
             #"Unexpected end of string"
             (parse* "'foo")))

      (t/is (thrown-with-msg?
             ExceptionInfo
             #"Unexpected end of string"
             (parse* "\"foo'"))))

    (t/testing "found escaping character without followup symbol"
      (t/is (thrown-with-msg?
             ExceptionInfo
             #"Unexpected end of string"
             (parse* "'foo\\"))))

    (t/is (= true
             (parse* "TRUE")))

    (t/is (= true
             (parse* "True")))

    (t/is (= true
             (parse* "true")))

    (t/is (= false
             (parse* "FALSE")))

    (t/is (= false
             (parse* "False")))

    (t/is (= false
             (parse* "false")))))

(t/deftest parse-arrays

  (t/is (= [1 2 3]
           (parse* "{1, 2, 3}")))

  (t/is (= [1 2 3]
           (parse* "{1,(1+1),3}")))

  (t/is (= [1 2 3]
           (parse* "{1,2,3,}"))))

(t/deftest parse-references

  (t/testing "symbols resolved as references during parsing"

    (t/is (= 1
             (parse* "foo" {:foo 1})))

    (t/is (= 1
             (parse* "foo\\+" {:foo+ 1})))

    (t/is (thrown-with-msg?
           ExceptionInfo
           #"Unexpected end of token"
           (parse* "foo\\" {:foo 1})))

    (try
      (parse* "foo\\")
      (catch ExceptionInfo e
        (let [data (ex-data e)]
          (t/is (= {:position
                    {:begin #::l{:line 1, :column 4}
                     :end #::l{:line 1, :column 4}}}
                   data)))))

    (t/is (thrown-with-msg?
           ExceptionInfo
           #"Unexpected end of reference expression"
           (parse* "foo.bar.")))

    (try
      (parse* "foo.bar.")
      (catch ExceptionInfo e
        (let [data (ex-data e)]
          (t/is (= {:position
                    {:begin #::l{:line 1, :column 1},
                     :end #::l{:line 1, :column 8}}}
                   data)))))

    (t/is (= 1
             (parse* "foo.bar" {:foo {:bar 1}})))

    (t/is (= 1
             (parse* "foo.'bar'" {:foo {:bar 1}})))

    (t/is (= 1
             (parse* "foo.\"bar\"" {:foo {"bar" 1}}))))

  (t/testing "index references resolved during parsing"

    (t/is (= 1
             (parse* "foo[0]" {"foo" [1 2 3]})))

    (t/is (= 1
             (parse* "foo.[0]" {:foo [1 2 3]})))

    (t/is (= nil
             (parse* "foo.[5]" {:foo [1 2 3]})))

    (t/is (= [1 2 3]
             (parse* "foo[*]" {:foo [1 2 3]})))

    (t/is (= [1 2 3]
             (parse* "foo.[*]" {:foo [1 2 3]})))

    (t/is (= [1 2 3]
             (parse* "foo.[  *  ]" {:foo [1 2 3]})))

    (t/is (= [1 2 3]
             (parse* "foo.[]" {:foo [1 2 3]})))

    (t/is (= [1 2 3]
             (parse* "foo.[].bar" {:foo [{:bar 1} {:bar 2} {:bar 3}]})))

    (t/is (= nil
             (parse* "foo[*]" {:foo {:bar 1}})))

    (t/is (= [{:foo 1} {:foo 2} {:foo 3}]
             (parse* "foo[*]" {:foo [{:foo 1} {:foo 2} {:foo 3}]})))

    (t/is (= [1 2 3]
             (parse* "foo.[*].foo" {:foo [{:foo 1} {:foo 2} {:foo 3}]})))

    (t/is (= [1 nil 3]
             (parse* "foo[*].foo" {:foo [{:foo 1} {:bar 2} {:foo 3}]}))))

  (t/testing "references can contains keywords"

    (t/is (= 1
             (parse* ":foo/bar" {:foo/bar 1})))

    (t/is (= 1
             (parse* ":foo.bar/baz.booz" {:foo.bar/baz {:booz 1}}))))

  (t/testing "references can begin with indexing operation"

    (t/is (= 1
             (parse* "[0].foo" [{:foo 1}])))

    (t/is (= [1 2 3]
             (parse* "[*].foo" [{:foo 1} {:foo 2} {:foo 3}])))))

(t/deftest parse-prefix-expressions

  (t/is (= false
           (parse* "!1")))

  (t/is (= true
           (parse* "!False")))

  (t/is (= 1
           (parse* "+1")))

  (t/is (= -1
           (parse* "-1")))

  (t/is (= -2
           (parse* "-(1 + 1)"))))

(t/deftest parse-postfix-expression

  (t/is (= 0.01
           (parse* "1%")))

  (t/is (= 0.02
           (parse* "(1 + 1)%"))))

(t/deftest parse-infix-expressions

  (t/is (= 2
           (parse* "1 + 1")))

  (t/is (= 6
           (parse* "2 + 2 * 2")))

  (t/is (= 6
           (parse* "2*2+2")))

  (t/is (= 8
           (parse* "(2 + 2) * 2"))))

(t/deftest parse-functional-application

  (t/is (= 1
           (parse* "IF(TRUE, 1, 2)")))

  (t/is (= [1]
           (parse* "{IF(FALSE, 2, 1)}"))))

(t/deftest errors

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Unexpected token"
         (parse* "1 + 1 1")))

  (try
    (parse* "1 + 1 1")
    (catch ExceptionInfo e
      (let [data (ex-data e)]
        (t/is (= {:position
                  #::l{:begin #::l{:line 1, :column 7},
                       :end #::l{:line 1, :column 7}}}
                 data))))))
