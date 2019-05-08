(ns axel-f.reference-test
  (:require [axel-f.excel :as af]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest list-refs
  (t/is (= '(1 2 3)
           ((af/compile "_.[*]") [1 2 3])))

  (t/is (= 2
           ((af/compile "_.[1 + 0]") [1 2 3])))

  (t/is (= '(1 2 3)
           ((af/compile "foo.[].bar") {:foo [{:bar 1}
                                             {:bar 2}
                                             {:bar 3}]}))))

(t/deftest reference-can-start-with-string
  (t/is (= 1
           ((af/compile "'foo'.bar") {:foo {:bar 1}}))))

(t/deftest reference-with-keywords
  (t/are [x] (t/is (= 1 ((af/compile (str "0 + " x " * 1")) {:foo 1
                                                             :foo/bar 1
                                                             :foo.bar/baz 1
                                                             :foo.bar.baz/buz 1})))
    :foo
    :foo/bar
    :foo.bar/baz
    :foo.bar.baz/buz))

(t/deftest base-environment-not-returned
  (t/is (= nil
           ((af/compile "OBJECT"))))

  (t/is (= nil
           ((af/compile "MAP"))))

  (t/is (= nil
           ((af/compile "WITH(x, MAP, x)")))))
