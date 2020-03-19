(ns axel-f.excel.xpath
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
  (->> (string/split query #"/")
       (remove empty?)
       (map parse-item)
       (map #(comp (tag= %) (attr= %) children))
       (apply comp)))

(defn xpath-find [items query]
  (let [query (or query "/")
        items (if (sequential? items) items [items])
        tag-getter (make-tag-getter query)]
    (sequence tag-getter items)))

(defn ->xml-like [obj]
  (cond
    (map-entry? obj)
    [{"tag" (first obj)
      "children" (->xml-like (second obj))}]

    (or (sequential? obj)
        (map? obj))
    (mapcat ->xml-like obj)

    :else
    (list obj)))

(defn TAGGED*
  "Convert arbitrary collection into tagged form to perform XPATH.FIND over

  {\"foo\": [1, 2, 3]} => {\"tag\": \"foo\", \"children\": [1, 2, 3]}"
  [^{:doc "Object to convert into xml-like structure"} obj]
  (->xml-like obj))

(def TAGGED #'TAGGED*)

(defn FIND*
  "Perform xpath-like query on collection of items"
  [^{:doc "Object representaiton of xml-like structure"} items
   ^{:doc "Query to perform on `items`"} query]
  (xpath-find items query))

(def FIND #'FIND*)

(def env
  {"XPATH" {"FIND"   FIND
            "TAGGED" TAGGED}})
