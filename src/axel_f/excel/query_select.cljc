(ns axel-f.excel.query-select
  (:require [clojure.string :as string]))

(def children (mapcat #(get % "children")))

(defn tag= [{:keys [tag]}]
  (filter #(= tag (get % "tag"))))

(defn attr= [{:keys [attr value]}]
  (if (some? attr)
    (filter #(= value
                (get-in % ["attrs" attr])))
    identity))

(defn parse-item [item]
  (if-some [[_ tag attr value] (re-matches #"(.*)\[(.*)=(.*)\]" item)]
    {:tag tag
     :attr attr
     :value value}
    {:tag item}))

(defn make-tag-getter [query]
  (->> (string/split query #"\.")
       (map parse-item)
       (map #(comp (tag= %) (attr= %) children))
       (apply comp)))

(defn query-select [items query]
  (let [query (or query "")
        tag-getter (make-tag-getter query)]
    (sequence tag-getter items)))

(defn QUERYSELECT*
  "Perform querySelector-like query on collection of items"
  [^{:doc "Object representaiton of xml-like structure"} item
   ^{:doc "Query to perform on `item`"} query]
  (let [items (if (sequential? item) item [item])]
    (query-select items query)))

(def QUERYSELECT #'QUERYSELECT*)

(def env
  {"QUERYSELECT" QUERYSELECT})

(comment
  (let [_ "<?xml version= \"1.0\"?><response><id-number>2716902077</id-number><summary-result><key>id.success</key><message>PASS</message></summary-result><results><key>result.match</key><message>ID Located</message></results></response>"
        parsed2 {"tag" "response","attrs" {"foo" "bar"} "children" [{"tag" "id-number", "children" ["2716902077\n  "]} {"tag" "summary-result", "children" [{"tag" "key", "children" ["id.success\n    "]} {"tag" "message", "children" ["PASS"]} {"tag" "message", "children" ["PASS22"]}]} {"tag" "results", "children" [{"tag" "key", "children" ["result.match\n    "]} {"tag" "message", "children" ["ID Located\n    "]}]}]}
        items [parsed2]
        query "response[foo=bar].summary-result.message"]
    (query-select items query)))
