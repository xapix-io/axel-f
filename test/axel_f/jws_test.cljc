(ns axel-f.jws-test
  (:require [axel-f.excel :as af]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest jws-hs256
  #_(t/testing "Sign payload using HS256"
    (t/is (= "eyJhbGciOiJIUzI1NiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.HU45XthYzICLPj8RvTeVQum2FLPdynx0MTsSCs5l-O0"
             ((af/compile "JWS.SIGN('HS256', JSON.ENCODE(OBJECT.NEW({{\"foo\", 1}, {\"bar\", {4, 5, 'qwe'}}})), 'password')")))))

  (t/testing "Extract payload using HS256"
    (t/is (= "{\"foo\":1,\"bar\":[4,5,\"qwe\"]}"
             ((af/compile "JWS.EXTRACT('HS256', 'eyJhbGciOiJIUzI1NiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.HU45XthYzICLPj8RvTeVQum2FLPdynx0MTsSCs5l-O0', 'password')")))))

  (t/testing "Verify and extract payload using HS256"
    (t/is (= {"payload" "{\"foo\":1,\"bar\":[4,5,\"qwe\"]}"}
             ((af/compile "JWS.VERIFY('HS256', 'eyJhbGciOiJIUzI1NiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.HU45XthYzICLPj8RvTeVQum2FLPdynx0MTsSCs5l-O0', 'password')"))))))

(t/deftest jws-hs384
  #_(t/testing "Sign payload using HS384"
    (t/is (= "eyJhbGciOiJIUzM4NCJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.-zGxO2ktyYtuodycQbEE8tHGv24aBZc8o5O--pvARuuIHFhw4fZBU-u_npx7hNvb"
             ((af/compile "JWS.SIGN('HS384', JSON.ENCODE(OBJECT.NEW({{\"foo\", 1}, {\"bar\", {4, 5, 'qwe'}}})), 'password')")))))

  (t/testing "Extract payload using HS384"
    (t/is (= "{\"foo\":1,\"bar\":[4,5,\"qwe\"]}"
             ((af/compile "JWS.EXTRACT('HS384', 'eyJhbGciOiJIUzM4NCJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.-zGxO2ktyYtuodycQbEE8tHGv24aBZc8o5O--pvARuuIHFhw4fZBU-u_npx7hNvb', 'password')")))))

  (t/testing "Verify and extract payload using HS384"
    (t/is (= {"payload" "{\"foo\":1,\"bar\":[4,5,\"qwe\"]}"}
             ((af/compile "JWS.VERIFY('HS384', 'eyJhbGciOiJIUzM4NCJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.-zGxO2ktyYtuodycQbEE8tHGv24aBZc8o5O--pvARuuIHFhw4fZBU-u_npx7hNvb', 'password')"))))))

(t/deftest jws-hs512
  #_(t/testing "Sign payload using HS512"
    (t/is (= "eyJhbGciOiJIUzUxMiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.guAH0rsu-o6AJsUvilGRxbi74g0xhDxOP9SCxuTUooPiAdWK0Vl2WKsb9S-5dJ0n2qgol7uZJQWmFp6R4uskcg"
             ((af/compile "JWS.SIGN('HS512', JSON.ENCODE(OBJECT.NEW({{\"foo\", 1}, {\"bar\", {4, 5, 'qwe'}}})), 'password')")))))

  (t/testing "Extract payload using HS512"
    (t/is (= "{\"foo\":1,\"bar\":[4,5,\"qwe\"]}"
             ((af/compile "JWS.EXTRACT('HS512', 'eyJhbGciOiJIUzUxMiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.guAH0rsu-o6AJsUvilGRxbi74g0xhDxOP9SCxuTUooPiAdWK0Vl2WKsb9S-5dJ0n2qgol7uZJQWmFp6R4uskcg', 'password')")))))

  (t/testing "Verify and extract payload using HS512"
    (t/is (= {"payload" "{\"foo\":1,\"bar\":[4,5,\"qwe\"]}"}
             ((af/compile "JWS.VERIFY('HS512', 'eyJhbGciOiJIUzUxMiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.guAH0rsu-o6AJsUvilGRxbi74g0xhDxOP9SCxuTUooPiAdWK0Vl2WKsb9S-5dJ0n2qgol7uZJQWmFp6R4uskcg', 'password')"))))))

(t/deftest jws-es256
  (def privkey "-----BEGIN EC PRIVATE KEY-----
MHcCAQEEIPlxv+fqINGK0SvHWA3Ik77M842CVDY0jFa92x6i4tFCoAoGCCqGSM49
AwEHoUQDQgAESSSHU2Q0YA3u9xEYVNX9UzfE/I5BfTN6I2ChVc6gci6XL0s4d50i
+/pmvEGr8DQAPKeCsTr1jZ4x8K8KJo1uJA==
-----END EC PRIVATE KEY-----")

  (def pubkey "-----BEGIN PUBLIC KEY-----
MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAESSSHU2Q0YA3u9xEYVNX9UzfE/I5B
fTN6I2ChVc6gci6XL0s4d50i+/pmvEGr8DQAPKeCsTr1jZ4x8K8KJo1uJA==
-----END PUBLIC KEY-----")

  #_(t/testing "Sign payload using ES256"
    (t/is (clojure.string/starts-with? ((af/compile "JWS.SIGN('ES256', JSON.ENCODE(OBJECT.NEW({{\"foo\", 1}, {\"bar\", {4, 5, 'qwe'}}})), KEY.PRIV(keys.priv))") {"keys" {"priv" privkey}})
                                       "eyJhbGciOiJFUzI1NiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.")))

  (t/testing "Extract payload using ES256"
    (t/is (= "{\"foo\":1,\"bar\":[4,5,\"qwe\"]}"
             ((af/compile "JWS.EXTRACT('ES256', 'eyJhbGciOiJFUzI1NiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.SOuE2pNPBLJHBct8cBcJAZ0euu-RTskg4yaOXMGtSdrBaaSIbMnJKproJOb63ngQKwtuv4W7fUCuEHpARAOq-Q', KEY.PUB(keys.pub))") {"keys" {"pub" pubkey}}))))

  (t/testing "Verify and extract payload using ES256"
    (t/is (= {"payload" "{\"foo\":1,\"bar\":[4,5,\"qwe\"]}"}
             ((af/compile "JWS.VERIFY('ES256', 'eyJhbGciOiJFUzI1NiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.SOuE2pNPBLJHBct8cBcJAZ0euu-RTskg4yaOXMGtSdrBaaSIbMnJKproJOb63ngQKwtuv4W7fUCuEHpARAOq-Q', KEY.PUB(keys.pub))") {"keys" {"pub" pubkey}})))))

(t/deftest errors
  (t/testing "mismatch algorithms"
    (t/is (= {"error" {"type" 0 "message" "Algorithm mismatch"}}
             ((af/compile "JWS.VERIFY('HS384', 'eyJhbGciOiJIUzI1NiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.HU45XthYzICLPj8RvTeVQum2FLPdynx0MTsSCs5l-O0', 'password')")))))

  (t/testing "wrong signature"
    (t/is (= {"error" {"type" 1 "message" "Message seems corrupt or modified"}}
             ((af/compile "JWS.VERIFY('HS256', 'eyJhbGciOiJIUzI1NiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.HU45XthYzICLPj8RvTeVQum2FCs5l-O1', 'password')"))))

    (t/is (= {"error" {"type" 1 "message" "Message seems corrupt or modified"}}
             ((af/compile "JWS.VERIFY('HS256', 'eyJhbGciOiJIUzI1NiJ9.eyJmb28iOjEsImJhciI6WzQsNSwicXdlIl19.HU45XthYzICLPj8RvTeVQum2FLPdynx0MTsSCs5l-O0', 'passwor')"))))))
