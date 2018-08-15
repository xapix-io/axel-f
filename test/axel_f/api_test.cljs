(ns axel-f.api-test
  (:require [axel-f.api :as sut]
            [cljs.test :as t :include-macros true]))

(t/deftest compile-test

  (t/testing "compile function return javascript array"

    (t/is (= ["OBJREF" "foo" "bar"]
             (js->clj (sut/compile "foo.bar"))))

    (t/is (= ["FNCALL" "SUM" [1 "foo"]]
             (js->clj (sut/compile "SUM(1, \"foo\")"))))))

(t/deftest run-test

  (t/testing "run function can process strings and javascript arrays"

    (t/is (= 1 (sut/run
                 "foo.bar"
                 (clj->js {:foo {:bar 1}}))))

    (t/is (= 1 (sut/run
                 (clj->js [:OBJREF "foo" "bar"])
                 (clj->js {:foo {:bar 1}}))))

    (t/is (= 1 (sut/run
                 (clj->js ["OBJREF" "foo" "bar"])
                 (clj->js {:foo {:bar 1}}))))))
