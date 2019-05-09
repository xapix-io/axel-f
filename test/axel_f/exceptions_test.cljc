(ns axel-f.exceptions-test
  (:require [axel-f.excel :as af]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true]))
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(t/deftest no-expression-inside-round-block

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Empty expression inside block."
         ((af/compile "1 + ()"))))

  (try
    ((af/compile "1 + ()"))
    (catch #?(:clj ExceptionInfo
              :cljs js/Error) e
      (let [d (ex-data e)]
        (t/is (= {:begin
                  #:axel-f.lexer{:line 1
                                 :col 5}
                  :end
                  #:axel-f.lexer{:line 1
                                 :col 6}
                  :axel-f.excel/formula "1 + ()"}
                 d))))))

(t/deftest unclosed-round-bracket

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Unclosed round bracket."
         ((af/compile "2 * (2 + 2 "))))

  (try
    ((af/compile "2 * (2 + 2"))
    (catch #?(:clj ExceptionInfo
              :cljs js/Error) e
      (let [d (ex-data e)]
        (t/is (= {:begin
                  #:axel-f.lexer{:line 1
                                 :col 5}
                  :end
                  #:axel-f.lexer{:line 1
                                 :col 11}
                  :axel-f.excel/formula "2 * (2 + 2"}
                 d))))))

(t/deftest multiple-expressions-inside-square-block

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Multiple expressions detected."
         ((af/compile "foo[1 + 1 2]"))))

  (try
    ((af/compile "foo[1 + 1 2]"))
    (catch #?(:clj ExceptionInfo
              :cljs js/Error) e
      (let [d (ex-data e)]
        (t/is (= {:begin
                  #:axel-f.lexer{:line 1
                                 :col 4}
                  :end
                  #:axel-f.lexer{:line 1
                                 :col 11}
                  :axel-f.excel/formula "foo[1 + 1 2]"}
                 d))))))

(t/deftest unclosed-square-block

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Unclosed square bracket."
         ((af/compile "foo[1"))))

  (try
    ((af/compile "foo[1"))
    (catch #?(:clj ExceptionInfo
              :cljs js/Error) e
      (let [d (ex-data e)]
        (t/is (= {:begin
                  #:axel-f.lexer{:line 1
                                 :col 4}
                  :end
                  #:axel-f.lexer{:line 1
                                 :col 6}
                  :axel-f.excel/formula "foo[1"}
                 d))))))

(t/deftest invalid-operator-in-square-block

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Invalid operator inside array reference expression."
         ((af/compile "foo[-]"))))

  (try
    ((af/compile "foo[-]"))
    (catch #?(:clj ExceptionInfo
              :cljs js/Error) e
      (let [d (ex-data e)]
        (t/is (= {:begin #:axel-f.lexer{:line 1
                                        :col 5}
                  :axel-f.excel/formula "foo[-]"}
                 d))))))

(t/deftest unclosed-comment-block

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Unclosed comment block"
         ((af/compile "1 ;~ Unclosed comment block"))))

  (try
    ((af/compile "1 ;~ Unclosed comment block"))
    (catch #?(:clj ExceptionInfo
              :cljs js/Error) e
      (let [d (ex-data e)]
        (t/is (= {:begin #:axel-f.lexer{:line 1
                                        :col 27}
                  :axel-f.excel/formula "1 ;~ Unclosed comment block"}
                 d))))))

(t/deftest unclosed-curly-block

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Unexpected end of input"
         ((af/compile "{1, 2, "))))

  (try
    ((af/compile "{1, 2, "))
    (catch #?(:clj ExceptionInfo
              :cljs js/Error) e
      (let [d (ex-data e)]
        (t/is (= {:begin #:axel-f.lexer{:line 1
                                        :col 8}
                  :axel-f.excel/formula "{1, 2, "}
                 d))))))

(t/deftest eof-in-string

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Unexpected end of string"
         ((af/compile "1 & ' asd"))))

  (try
    ((af/compile "1 & ' asd"))
    (catch #?(:clj ExceptionInfo
              :cljs js/Error) e
      (let [d (ex-data e)]
        (t/is (= {:begin #:axel-f.lexer{:line 1
                                        :col 10}
                  :axel-f.excel/formula "1 & ' asd"}
                 d)))))

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Unexpected end of string"
         ((af/compile "1 & \" asd"))))

  (try
    ((af/compile "1 & \" asd"))
    (catch #?(:clj ExceptionInfo
              :cljs js/Error) e
      (let [d (ex-data e)]
        (t/is (= {:begin #:axel-f.lexer{:line 1
                                        :col 10}
                  :axel-f.excel/formula "1 & \" asd"}
                 d))))))

(t/deftest multiple-top-level-expressions

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Unexpected token"
         ((af/compile "1 + 1 2 * 2"))))

  (try
    ((af/compile "1 + 1 2 * 2"))
    (catch #?(:clj ExceptionInfo
              :cljs js/Error) e
      (let [d (ex-data e)]
        (t/is (= {:begin
                  #:axel-f.lexer{:line 1
                                 :col 7}
                  :axel-f.excel/formula "1 + 1 2 * 2"}
                 d))))))

(t/deftest no-operator-implementation

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Operator '\+' doesn't have implementation\."
         ((af/compile "1 + 1" {"+" nil}))))

  (try
    ((af/compile "1 + 1" {"+" nil}))
    (catch #?(:clj ExceptionInfo
              :cljs js/Error) e
      (let [d (ex-data e)]
        (t/is (= {:begin #:axel-f.lexer{:line 1 :col 3}}
                 d))))))

(t/deftest wrong-argument-symbol-for-fn

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Wrong argument symbol: `x\.y`"
         ((af/compile "FN(x.y, 1)"))))

  (try
    ((af/compile "FN(x.y, 1)"))
    (catch #?(:clj ExceptionInfo
              :cljs js/Error) e
      (let [d (ex-data e)]
        (t/is (= {:begin #:axel-f.lexer{:col 4 :line 1}
                  :axel-f.excel/formula "FN(x.y, 1)"}
                 d))))))

(t/deftest invalid-namespaced-keyword

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Namespaced keyword must have a name"
         ((af/compile ":foo.bar/"))))

  (try
    ((af/compile ":foo.bar/"))
    (catch #?(:clj ExceptionInfo
              :cljs js/Error) e
      (let [data (ex-data e)]
        (t/is (= {:begin #:axel-f.lexer{:line 1, :col 1},
                  :end #:axel-f.lexer{:line 1, :col 9}
                  :axel-f.excel/formula ":foo.bar/"}
                 data))))))
