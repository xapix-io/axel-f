(ns axel-f.hash-test
  (:require [axel-f.excel :as af]
            [axel-f.excel.hash :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest sha256
  (t/testing "Digest string"

    (t/is (= "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae"
             (sut/sha256 "foo")))))

(t/deftest hash-sha256
  (let [f (af/compile "HASH.SHA256('foo')")]
    (t/is (= "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae" (f)))))

(t/deftest sha224
  (t/testing "Digest string"

    (t/is (= "0808f64e60d58979fcb676c96ec938270dea42445aeefcd3a4e6f8db"
             (sut/sha224 "foo")))))

(t/deftest hash-sha224
  (let [f (af/compile "HASH.SHA224('foo')")]
    (t/is (= "0808f64e60d58979fcb676c96ec938270dea42445aeefcd3a4e6f8db" (f)))))

(t/deftest sha384
  (t/testing "Digest string"

    (t/is (= "98c11ffdfdd540676b1a137cb1a22b2a70350c9a44171d6b1180c6be5cbb2ee3f79d532c8a1dd9ef2e8e08e752a3babb"
             (sut/sha384 "foo")))))

(t/deftest hash-sha384
  (let [f (af/compile "HASH.SHA384('foo')")]
    (t/is (= "98c11ffdfdd540676b1a137cb1a22b2a70350c9a44171d6b1180c6be5cbb2ee3f79d532c8a1dd9ef2e8e08e752a3babb" (f)))))

(t/deftest sha512
  (t/testing "Digest string"

    (t/is (= "f7fbba6e0636f890e56fbbf3283e524c6fa3204ae298382d624741d0dc6638326e282c41be5e4254d8820772c5518a2c5a8c0c7f7eda19594a7eb539453e1ed7"
             (sut/sha512 "foo")))))

(t/deftest hash-sha512
  (let [f (af/compile "HASH.SHA512('foo')")]
    (t/is (= "f7fbba6e0636f890e56fbbf3283e524c6fa3204ae298382d624741d0dc6638326e282c41be5e4254d8820772c5518a2c5a8c0c7f7eda19594a7eb539453e1ed7" (f)))))

(t/deftest sha1
  (t/testing "Digest string"

    (t/is (= "0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33"
             (sut/sha1 "foo")))))

(t/deftest hash-sha1
  (let [f (af/compile "HASH.SHA1('foo')")]
    (t/is (= "0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33" (f)))))

(t/deftest md5
  (t/testing "Digest string"

    (t/is (= "acbd18db4cc2f85cedef654fccc4a4d8"
             (sut/md5 "foo")))))

(t/deftest hash-md5
  (let [f (af/compile "HASH.MD5('foo')")]
    (t/is (= "acbd18db4cc2f85cedef654fccc4a4d8" (f)))))
