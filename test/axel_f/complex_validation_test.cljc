(ns axel-f.complex-validation-test
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [axel-f.excel :as af]
            [axel-f.excel.validate :as validate])
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(t/deftest coercion-and-validation

  (try
    ((af/compile "validate.presence(foo.bar)"))
    (catch #?(:clj ExceptionInfo :cljs js/Error) e
      (t/is (= "Argument required" (.-message e)))
      (t/is (= {:type ::validate/validate-error
                :subtype ::validate/presence}
               (ex-data e)))))

  (try
    ((af/compile "validate.presence(foo.bar, 'Query param missing')"))
    (catch #?(:clj ExceptionInfo :cljs js/Error) e
      (t/is (= "Query param missing" (.-message e)))))

  (t/is (= 123 ((af/compile "validate.presence(foo.bar)") {"foo" {"bar" 123}})))

  (try
    ((af/compile "validate.not-empty(foo.bar)") {"foo" {"bar" ""}})
    (catch #?(:clj ExceptionInfo :cljs js/Error) e
      (t/is (= "Argument can not be empty" (.-message e)))
      (t/is (= {:type ::validate/validate-error
                :subtype ::validate/empty}
               (ex-data e)))))

  (try
    ((af/compile "validate.not-empty(foo.bar, 'Query param is empty')") {"foo" {"bar" {}}})
    (catch #?(:clj ExceptionInfo :cljs js/Error) e
      (t/is (= "Query param is empty" (.-message e)))))

  (t/is (= 123 ((af/compile "validate.not-empty(foo.bar)") {"foo" {"bar" 123}})))


  (t/is (= 1 ((af/compile "coerce.to-integer('1')"))))
  (t/is (= 1 ((af/compile "coerce.to-integer(1)"))))
  (t/is (= "123" ((af/compile "coerce.to-string(123)"))))
  (t/is (= nil ((af/compile "coerce.to-string(NULL)"))))
  (t/is (= 3.14 ((af/compile "coerce.to-float('3.14')"))))
  (t/is (= 3.14 ((af/compile "coerce.to-float(3.14)"))))
  (t/is (= true ((af/compile "coerce.to-boolean('true')"))))
  (t/is (= nil
           ((af/compile "coerce.to-integer('qwe')"))
           ((af/compile "coerce.to-float('qwe')"))
           ((af/compile "coerce.to-boolean('qwe')")))))

(t/deftest complex-coercion-and-validation
  (t/is (= 1 ((af/compile "walk(composition(coerce.to-integer), 0, foo.bar)") {"foo" {"bar" "1"}})))
  (t/is (= 1 ((af/compile "walk(composition(validate.presence, coerce.to-integer), 0, foo.bar)") {"foo" {"bar" "1"}})))
  (t/is (= nil ((af/compile "walk(composition(coerce.to-integer), 0, foo.bar)") {"foo" {"bar" "qwe"}})))

  (try
    ((af/compile "walk(composition(validate.presence, coerce.to-integer), 0, foo.bar)") {"foo" {}})
    (catch #?(:clj ExceptionInfo :cljs js/Error) e
      (t/is (= "Argument required" (.-message e)))))

  (t/is (= [1 2 3]
           ((af/compile "walk(composition(coerce.to-integer), 1, foo[*].bar)")
            {"foo" [{"bar" "1"}
                    {"bar" "2"}
                    {"bar" "3"}]})))
  (t/is (= [[1 2 3] [4 5 6]]
           ((af/compile "walk(composition(coerce.to-integer), 2, foo[*].bar[*])")
            {"foo" [{"bar" ["1" "2" "3"]}
                    {"bar" ["4" "5" "6"]}]})))

  (t/is (= [[1 nil 3] [4 5 6]]
           ((af/compile "walk(composition(coerce.to-integer), 2, foo[*].bar[*])")
            {"foo" [{"bar" ["1" "qwe" "3"]}
                    {"bar" ["4" "5" "6"]}]})))

  (try
    ((af/compile "walk(composition(validate.presence, coerce.to-integer), 2, foo[*].bar[*])")
     {"foo" [{"bar" ["1" "qwe" "3"]}
             {"bar" ["4" "5" "6"]}]})
    (catch #?(:clj ExceptionInfo :cljs js/Error) e
      (t/is (= "Argument required" (.-message e)))))

  (t/is (= [1 2 3]
           ((af/compile "walk(composition(validate.not-empty), 1, foo[*].bar)")
            {"foo" [{"bar" 1}
                    {"bar" 2}
                    {"bar" 3}]})))

  (try
    ((af/compile "walk(composition(validate.not-empty), 1, foo[*].bar)")
     {"foo" [{"barz" 1}
             {"barz" 2}
             {"barz" 3}]})
    (catch #?(:clj ExceptionInfo :cljs js/Error) e
      (t/is (= "Argument can not be empty" (.-message e)))))

  (t/is (= {"users" [{"id" 1
                      "projects" [{"name" nil
                                   "ratings" [2 3]}]}
                     {"id" 2
                      "projects" [{"name" nil
                                   "ratings" [5 4]}]}]}
           ((af/compile "walk(coerce.to-integer, object)")
            {"object" {"users" [(sorted-map "id" "1"
                                            "projects" [{"name" "project2"
                                                         "ratings" ["2" "3"]}])
                                {"id" "2"
                                 "projects" [{"name" "project1"
                                              "ratings" ["5" "4"]}]}]}})))

  (t/is (= [[[2 3]] [[5 4]]]
           ((af/compile "walk(coerce.to-integer, users[].projects[].ratings)")
            {"users" [{"id" "1"
                       "projects" [{"name" "project2"
                                    "ratings" ["2" "3"]}]}
                      {"id" "2"
                       "projects" [{"name" "project1"
                                    "ratings" ["5" "4"]}]}]})))

  (t/is (= [[nil] [[nil 4]]]
           ((af/compile "walk(coerce.to-integer, users[].projects[].ratings)")
            {"users" [{"id" "1"
                       "projects" [{"name" "project2"}]}
                      {"id" "2"
                       "projects" [{"name" "project1"
                                    "ratings" ["5a" "4"]}]}]})))

  (t/is (= nil
           ((af/compile "walk(coerce.to-integer, users[].project[].ratings)")
            {"title" "hey"})))

  (try
    ((af/compile "walk(composition(validate.presence, coerce.to-integer), users[].projects[].ratings)")
     {"users" [{"id" "1"
                ;; no ratings here
                "projects" [{"name" "project2"}]}
               {"id" "2"
                "projects" [{"name" "project1"
                             "ratings" ["5" "4"]}]}]})
    (catch #?(:clj ExceptionInfo :cljs js/Error) e
      (t/is (= "Argument required" (.-message e)))))

  (t/is (= 18 ((af/compile "walk(composition(validate.presence, coerce.to-integer), user.age)")
               {"user" {"age" "18"}})))

  (t/is (= [18] ((af/compile "walk(composition(validate.presence, coerce.to-integer), user.age)")
                 {"user" {"age" ["18"]}})))

  (try
    ((af/compile "walk(composition(validate.presence, coerce.to-integer), 1, user.age)")
     {"user" {"age" ["18"]}})
    (catch #?(:clj ExceptionInfo :cljs js/Error) e
      (t/is (= "Argument required" (.-message e)))))

  (t/is (= [{"foo" 1 "oof" 2} {"foo" 3}]
           ((af/compile "walk(coerce.to-integer, foo[].bar)")
            {"foo" [{"bar" {"foo" "1" "oof" "2"}}
                    {"bar" {"foo" 3}}]}))))
