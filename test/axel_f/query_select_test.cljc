(ns axel-f.query-select-test
  (:require  #?(:clj [clojure.test :as t]
                :cljs [cljs.test :as t :include-macros true])
             [axel-f.excel :as af]))

(t/deftest query-select
  (let [xml {"tag" "response","attrs" {"foo" "bar"} "children" [{"tag" "id-number", "children" ["2716902077\n  "]} {"tag" "summary-result", "children" [{"tag" "key", "children" ["id.success\n    "]} {"tag" "message", "children" ["PASS"]} {"tag" "message", "children" ["PASS22"]}]} {"tag" "results", "children" [{"tag" "key", "children" ["result.match\n    "]} {"tag" "message", "children" ["ID Located\n    "]}]}]}]
    (t/is (= ["PASS" "PASS22"]
             ((af/compile "QUERYSELECT(xml, query)"
                          {"query" "response[foo=bar].summary-result.message"
                           "xml" xml}))))
    (t/is (= ["PASS" "PASS22"]
             ((af/compile "QUERYSELECT(xml, query)"
                          {"query" "response.summary-result.message"
                           "xml" xml}))))
    (t/is (= [{"tag" "key", "children" ["id.success\n    "]}
              {"tag" "message", "children" ["PASS"]}
              {"tag" "message", "children" ["PASS22"]}]
             ((af/compile "QUERYSELECT(xml, query)"
                          {"query" "response.summary-result"
                           "xml" xml}))))
    (t/is (= []
             ((af/compile "QUERYSELECT(xml, query)"
                          {"query" nil
                           "xml" xml}))))
    (t/is (= []
             ((af/compile "QUERYSELECT(xml, query)"
                          {"query" "doesnt.exist"
                           "xml" xml}))))
    (t/is (= []
             ((af/compile "QUERYSELECT(xml, query)"
                          {"query" "doesnt.exist"
                           "xml" xml}))))
    (t/is (= []
             ((af/compile "QUERYSELECT(xml, query)"
                          {"query" "failed[fuuuuu.query"
                           "xml" xml}))))))
