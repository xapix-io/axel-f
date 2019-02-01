(ns axel-f.error-test
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [axel-f.functions.text :as text]
            [axel-f.core :as sut])
  #?(:clj (:import clojure.lang.ExceptionInfo)))

(t/deftest value-function

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Fail to coerce `foo` to number."
         (text/value "foo")))

  (t/is (thrown?
         ExceptionInfo
         (text/value [1 2 3]))))

(t/deftest error-in-unary

  (let [f (sut/compile "-'asd'")]
    (t/is (thrown?
           ExceptionInfo
           (f))))

  (let [f (sut/compile "'qwe'%")]
    (t/is (thrown-with-msg?
           ExceptionInfo
           #"Formula error"
           (f))))

  (let [f (sut/compile "'foo'%")]
    (try
      (f)
      (catch ExceptionInfo e
        (let [data (ex-data e)]
          (t/is (= {:position
                    {:begin #:axel-f.lexer{:line 1, :column 1},
                     :end #:axel-f.lexer{:line 1, :column 6}}}
                   data)))))))

(t/deftest error-in-binary

  (let [f (sut/compile "2 - 'asd'")]
    (t/is (thrown-with-msg?
           ExceptionInfo
           #"Formula error"
           (f))))

  (let [f (sut/compile "2 + 'foo'")]
    (try
      (f)
      (catch ExceptionInfo e
        (let [data (ex-data e)]
          (t/is (= {:position
                    {:begin #:axel-f.lexer{:line 1, :column 1},
                     :end #:axel-f.lexer{:line 1, :column 9}}}
                   data)))))))

(t/deftest map-function

  (t/is (thrown? ExceptionInfo
                 ((sut/compile "MAP()"))))

  (try
    ((sut/compile "MAP(_)"))
    (catch ExceptionInfo e
      (t/is (= {:position
                {:begin #:axel-f.lexer{:line 1, :column 1},
                 :end #:axel-f.lexer{:line 1, :column 6}}}
               (ex-data e)))
      (t/is (= "Wrong number of arguments passed to `MAP` function."
               (.getMessage e))))))

(t/deftest filter-function

  (t/is (thrown? ExceptionInfo
                 ((sut/compile "FILTER()"))))

  (try
    ((sut/compile "FILTER(_)"))
    (catch ExceptionInfo e
      (t/is (= {:position
                {:begin #:axel-f.lexer{:line 1, :column 1},
                 :end #:axel-f.lexer{:line 1, :column 9}}}
               (ex-data e)))
      (t/is (= "Wrong number of arguments passed to `FILTER` function."
               (.getMessage e))))))

(t/deftest sort-function

  (t/is (thrown? ExceptionInfo
                 ((sut/compile "SORT()"))))

  (try
    ((sut/compile "SORT(_)"))
    (catch ExceptionInfo e
      (t/is (= {:position
                {:begin #:axel-f.lexer{:line 1, :column 1},
                 :end #:axel-f.lexer{:line 1, :column 7}}}
               (ex-data e)))
      (t/is (= "Wrong number of arguments passed to `SORT` function."
               (.getMessage e))))))

(t/deftest if-function

  (t/is (thrown? ExceptionInfo
                 ((sut/compile "IF()"))))

  (try
    ((sut/compile "IF(True)"))
    (catch ExceptionInfo e
      (t/is (= {:position
                {:begin #:axel-f.lexer{:line 1, :column 1},
                 :end #:axel-f.lexer{:line 1, :column 8}}}
               (ex-data e)))
      (t/is (= "Wrong number of arguments passed to `IF` function."
               (.getMessage e))))))

(t/deftest ifs-function

  (t/is (thrown? ExceptionInfo
                 ((sut/compile "IFS(True)"))))

  (try
    ((sut/compile "IFS(True, 1, False, 2, !1<>2)"))
    (catch ExceptionInfo e
      (t/is (= {:position
                {:begin #:axel-f.lexer{:line 1, :column 1},
                 :end #:axel-f.lexer{:line 1, :column 29}}}
               (ex-data e)))
      (t/is (= "Function `IFS` expecting even number of arguments"
               (.getMessage e))))))

(t/deftest unknown-function

  (t/is (thrown-with-msg?
         ExceptionInfo
         #"Unknown function `FOO.BAR`"
         ((sut/compile "FOO.BAR(1, 2, 3)"))))

  (try
    ((sut/compile "FOO.BAR(1,2,3)"))
    (catch ExceptionInfo e
      (t/is (= {:position
                {:begin #:axel-f.lexer{:line 1, :column 1},
                 :end #:axel-f.lexer{:line 1, :column 7}}}
               (ex-data e))))))

(t/deftest internal-exceptions

  (try
    ((sut/compile "SUM(VALUE(_), 2 3)") "qwe")
    (catch ExceptionInfo e
      (t/is (= {:position
                {:begin #:axel-f.lexer{:line 1, :column 5},
                 :end #:axel-f.lexer{:line 1, :column 12}},
                :arguments ["qwe"],
                :cause
                {:msg "Fail to coerce `qwe` to number.", :data {:type :argument-type}}}
               (ex-data e)))
      (t/is (= "Error in function `VALUE`"
               (.getMessage e))))))

(t/deftest args-count-check

  (try
    ((sut/compile "VALUE(1, 2)"))
    (catch ExceptionInfo e
      (t/is (= {:position
                {:begin #:axel-f.lexer{:line 1, :column 6},
                 :end #:axel-f.lexer{:line 1, :column 11}}}
               (ex-data e)))
      (t/is (= "Wrong number of arguments passed to `VALUE` function."
               (.getMessage e))))))
