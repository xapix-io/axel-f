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

(defn xpath [items query]
  (let [query (or query "/")
        items (if (sequential? items) items [items])
        tag-getter (make-tag-getter query)]
    (sequence tag-getter items)))

(defn XPATH*
  "Perform xpath-like query on collection of items"
  [^{:doc "Object representaiton of xml-like structure"} items
   ^{:doc "Query to perform on `items`"} query]
  (xpath items query))

(def XPATH #'XPATH*)

(def env
  {"XPATH" XPATH})
