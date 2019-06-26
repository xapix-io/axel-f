(ns axel-f.hash-test
  (:require [axel-f.excel :as af]
            [axel-f.excel.hash :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest sha256
  (t/testing "Digest string"

    (t/is (= "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae"
             (sut/sha256 "foo")))))

(t/deftest HASHSHA256
  (let [f (af/compile "HASH.SHA256('foo')")]
    (t/is (= "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae" (f)))))
