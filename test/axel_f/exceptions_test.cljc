(ns axel-f.exceptions-test
  (:require [axel-f.excel :as af]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true]))
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(t/deftest no-expression-inside-round-block
  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Can't extract expression."
         ((af/compile "1 + ()"))))

  (try
    ((af/compile "1 + ()"))
    (catch #?(:clj ExceptionInfo
              :cljs js/Error) e
      (let [d (ex-data e)]
        (t/is (= {:begin
                  #:axel-f.lexer{:type :axel-f.lexer/punct,
                                 :value ")",
                                 :line 1,
                                 :col 6,
                                 :length 1}}
                 d))))))

(t/deftest unclosed-round-bracket
  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Unclosed bracket."
         ((af/compile "2 * (2 + 2 "))))

  (try
    ((af/compile "2 * (2 + 2"))
    (catch #?(:clj ExceptionInfo
              :cljs js/Error) e
      (let [d (ex-data e)]
        (t/is (= {:begin
                  #:axel-f.lexer{:type :axel-f.lexer/punct,
                                 :value "(",
                                 :line 1,
                                 :col 5,
                                 :length 1},
                  :end #:axel-f.lexer{:type :axel-f.lexer/eof}}
                 d))))))

(t/deftest multiple-expressions-inside-square-block
  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Unclosed bracket."
         ((af/compile "foo[1 + 1 2]"))))

  (try
    ((af/compile "foo[1 + 1 2]"))
    (catch #?(:clj ExceptionInfo
              :cljs js/Error) e
      (let [d (ex-data e)]
        (t/is (= {:begin
                  #:axel-f.lexer{:type :axel-f.lexer/punct,
                                 :value "[",
                                 :line 1,
                                 :col 4,
                                 :length 1},
                  :end
                  #:axel-f.lexer{:type :axel-f.lexer/number,
                                 :value 2,
                                 :line 1,
                                 :col 11,
                                 :length 1}}
                 d))))))

(t/deftest unclosed-square-block
  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Unclosed bracket."
         ((af/compile "foo[1"))))

  (try
    ((af/compile "foo[1"))
    (catch #?(:clj ExceptionInfo
              :cljs js/Error) e
      (let [d (ex-data e)]
        (t/is (= {:begin
                  #:axel-f.lexer{:type :axel-f.lexer/punct,
                                 :value "[",
                                 :line 1,
                                 :col 4,
                                 :length 1},
                  :end #:axel-f.lexer{:type :axel-f.lexer/eof}}
                 d))))))

(t/deftest empty-square-block
  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Can't extract expression."
         ((af/compile "foo[]"))))

  (try
    ((af/compile "foo[]"))
    (catch #?(:clj ExceptionInfo
              :cljs js/Error) e
      (let [d (ex-data e)]
        (t/is (= {:begin
                  #:axel-f.lexer{:type :axel-f.lexer/punct,
                                 :value "[",
                                 :line 1,
                                 :col 4,
                                 :length 1}}
                 d))))))
