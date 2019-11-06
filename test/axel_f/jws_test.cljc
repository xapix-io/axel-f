(ns axel-f.jws-test
  (:require [axel-f.excel :as af]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest jws-hs256
  (t/testing "Sign payload using HS256"
    (t/is (= "eyJhbGciOiJIUzI1NiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.HU45XthYzICLPj8RvTeVQum2FLPdynx0MTsSCs5l-O0"
             ((af/compile "JWS.SIGN('HS256', JSON.ENCODE(OBJECT.NEW({{\"foo\", 1}, {\"bar\", {4, 5, 'qwe'}}})), 'password')")))))

  (t/testing "Extract payload using HS256"
    (t/is (= "{\"foo\":1,\"bar\":[4,5,\"qwe\"]}"
             ((af/compile "JWS.EXTRACT('HS256', 'eyJhbGciOiJIUzI1NiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.HU45XthYzICLPj8RvTeVQum2FLPdynx0MTsSCs5l-O0', 'password')"))))))

(t/deftest jws-hs384
  (t/testing "Sign payload using HS384"
    (t/is (= "eyJhbGciOiJIUzM4NCJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.-zGxO2ktyYtuodycQbEE8tHGv24aBZc8o5O--pvARuuIHFhw4fZBU-u_npx7hNvb"
             ((af/compile "JWS.SIGN('HS384', JSON.ENCODE(OBJECT.NEW({{\"foo\", 1}, {\"bar\", {4, 5, 'qwe'}}})), 'password')")))))

  (t/testing "Extract payload using HS384"
    (t/is (= "{\"foo\":1,\"bar\":[4,5,\"qwe\"]}"
             ((af/compile "JWS.EXTRACT('HS384', 'eyJhbGciOiJIUzM4NCJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.-zGxO2ktyYtuodycQbEE8tHGv24aBZc8o5O--pvARuuIHFhw4fZBU-u_npx7hNvb', 'password')"))))))

(t/deftest jws-hs512
  (t/testing "Sign payload using HS512"
    (t/is (= "eyJhbGciOiJIUzUxMiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.guAH0rsu-o6AJsUvilGRxbi74g0xhDxOP9SCxuTUooPiAdWK0Vl2WKsb9S-5dJ0n2qgol7uZJQWmFp6R4uskcg"
             ((af/compile "JWS.SIGN('HS512', JSON.ENCODE(OBJECT.NEW({{\"foo\", 1}, {\"bar\", {4, 5, 'qwe'}}})), 'password')")))))

  (t/testing "Extract payload using HS512"
    (t/is (= "{\"foo\":1,\"bar\":[4,5,\"qwe\"]}"
             ((af/compile "JWS.EXTRACT('HS512', 'eyJhbGciOiJIUzUxMiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.guAH0rsu-o6AJsUvilGRxbi74g0xhDxOP9SCxuTUooPiAdWK0Vl2WKsb9S-5dJ0n2qgol7uZJQWmFp6R4uskcg', 'password')"))))))

(t/deftest errors
  (t/testing "missmatch algorithms"
    (t/is (= {:error {:type 0, :message "Algorithm missmatch"}}
             ((af/compile "JWS.VERIFY('HS384', 'eyJhbGciOiJIUzI1NiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.HU45XthYzICLPj8RvTeVQum2FLPdynx0MTsSCs5l-O0', 'password')")))))

  (t/testing "wrong signature"
    (t/is (= {:error {:type 1, :message "Message seems corrupt or modified"}}
             ((af/compile "JWS.VERIFY('HS256', 'eyJhbGciOiJIUzI1NiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.HU45XthYzICLPj8RvTeVQum2FCs5l-O1', 'password')"))))

    (t/is (= {:error {:type 1, :message "Message seems corrupt or modified"}}
             ((af/compile "JWS.VERIFY('HS256', 'eyJhbGciOiJIUzI1NiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.HU45XthYzICLPj8RvTeVQum2FLPdynx0MTsSCs5l-O0', 'passwor')"))))))
