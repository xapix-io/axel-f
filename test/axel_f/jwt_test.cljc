(ns axel-f.jwt-test
  (:require [axel-f.excel :as af]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest jwt-hs256
  (t/testing "Sign payload using HS256"
    (t/is (= "eyJhbGciOiJIUzI1NiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.HU45XthYzICLPj8RvTeVQum2FLPdynx0MTsSCs5l-O0"
             ((af/compile "JWT.SIGN('HS256', OBJECT.NEW({{\"foo\", 1}, {\"bar\", {4, 5, 'qwe'}}}), 'password')")))))

  (t/testing "Unsign payload using HS256"
    (t/is (= {"foo" 1 "bar" [4 5 "qwe"]}
             ((af/compile "JWT.UNSIGN('HS256', 'eyJhbGciOiJIUzI1NiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.HU45XthYzICLPj8RvTeVQum2FLPdynx0MTsSCs5l-O0', 'password')"))))))

(t/deftest jwt-hs384
  (t/testing "Sign payload using HS384"
    (t/is (= "eyJhbGciOiJIUzM4NCJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.-zGxO2ktyYtuodycQbEE8tHGv24aBZc8o5O--pvARuuIHFhw4fZBU-u_npx7hNvb"
             ((af/compile "JWT.SIGN('HS384', OBJECT.NEW({{\"foo\", 1}, {\"bar\", {4, 5, 'qwe'}}}), 'password')")))))

  (t/testing "Unsign payload using HS384"
    (t/is (= {"foo" 1 "bar" [4 5 "qwe"]}
             ((af/compile "JWT.UNSIGN('HS384', 'eyJhbGciOiJIUzM4NCJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.-zGxO2ktyYtuodycQbEE8tHGv24aBZc8o5O--pvARuuIHFhw4fZBU-u_npx7hNvb', 'password')"))))))

(t/deftest jwt-hs512
  (t/testing "Sign payload using HS512"
    (t/is (= "eyJhbGciOiJIUzUxMiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.guAH0rsu-o6AJsUvilGRxbi74g0xhDxOP9SCxuTUooPiAdWK0Vl2WKsb9S-5dJ0n2qgol7uZJQWmFp6R4uskcg"
             ((af/compile "JWT.SIGN('HS512', OBJECT.NEW({{\"foo\", 1}, {\"bar\", {4, 5, 'qwe'}}}), 'password')")))))

  (t/testing "Unsign payload using HS512"
    (t/is (= {"foo" 1 "bar" [4 5 "qwe"]}
             ((af/compile "JWT.UNSIGN('HS512', 'eyJhbGciOiJIUzUxMiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.guAH0rsu-o6AJsUvilGRxbi74g0xhDxOP9SCxuTUooPiAdWK0Vl2WKsb9S-5dJ0n2qgol7uZJQWmFp6R4uskcg', 'password')"))))))
