(ns axel-f.arity-test
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [axel-f.excel :as af])
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(defn foo-1 [_x _y & _z])

(defn foo-2 [_x _y])

(t/deftest stdlib

  (t/testing "variadic args"

    (let [f (af/compile "FOO(1,2,3)" {"FOO" #'foo-1})]
      (t/is (nil? (f))))

    (let [f (af/compile "FOO(1)" {"FOO" #'foo-1})]
      (t/is
       (thrown-with-msg?
        ExceptionInfo
        #"Wrong number of arguments passed to a function foo-1"
        (f)))))

  (t/testing "fixed args"

    (let [f (af/compile "FOO(1)" {"FOO" #'foo-2})]
      (t/is
       (thrown?
        ExceptionInfo
        (f))))

    (let [f (af/compile "FOO(1,2,3,4)" {"FOO" #'foo-2})]
      (t/is
       (thrown?
        ExceptionInfo
        (f)))))

  (t/testing "lambda"

    (let [f (af/compile "WITH(FOO, FN(x,y,1), FOO(1))")]
      (t/is
       (thrown?
        ExceptionInfo
        (f))))

    (let [f (af/compile "WITH(FOO, FN(x,y,1), FOO(1,2,3,4))")]
      (t/is
       (thrown?
        ExceptionInfo
        (f)))))

  #?(:clj
     (t/testing "no meta"

       (let [f (af/compile "FOO(1,2)" {"FOO" (fn [_x])})]
         (t/is
          (thrown-with-msg?
           ExceptionInfo
           #"Wrong number of arguments"
           (f))))))

  (t/testing "no function"

    (let [f (af/compile "FOO(1,2)")]
      (t/is
       (thrown-with-msg?
        ExceptionInfo
        #"Unknown function FOO"
        (f))))))
