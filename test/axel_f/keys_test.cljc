(ns axel-f.keys-test
  (:require [axel-f.excel.keys :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest validate-key-format

  (let [key-string "-----BEGIN EC PRIVATE KEY-----
MIIBIAIBAQQYd8yhaE899FaH3sw8aD4F/vtpMVBLfVqmoIHKMIHHAgEBMCQGByqG
SM49AQECGQD////////////////////+//////////8wSwQY////////////////
/////v/////////8BBgiEj3COVoFyqdCPa7MyUdgp9RiJWvVaRYDFQDEaWhENd6z
eMS2XKlZHipXYwWaLgQxBH0pd4EAxlodoXg3FliNziuLSu6OIo8YljipDyJjczcz
S0nctmptyPmXisp2SKlDsAIZAP///////////////3pi0DHIP0KU9kDsEwIBAaE0
AzIABBsl8ZSGJqcUpVoP8zekF92DGqDBMERcHhCXmgPXchP+ljybXbzYKINgxbp5
0g9/pw==
-----END EC PRIVATE KEY-----"]

    (t/testing "normal private key format"

      (t/is (sut/x-509-format? key-string)))

    (t/testing "key with whitespaces around"

      (t/is (sut/str->privkey (str " \n\t" key-string " \n\t")))))

  (let [key-string "-----BEGIN PUBLIC KEY-----
MEkwEwYHKoZIzj0CAQYIKoZIzj0DAQMDMgAE+Y+qPqI3geo2hQH8eK7Rn+YWG09T
ejZ5QFoj9fmxFrUyYhFap6XmTdJtEi8myBmW
-----END PUBLIC KEY-----"]

    (t/testing "normal public key format"

      (t/is (sut/x-509-format? key-string)))

    (t/testing "key with whitespaces around"

      (t/is (sut/str->pubkey (str " \n\t" key-string " \n\t"))))))
