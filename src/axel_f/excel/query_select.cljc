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

(defn ->tag-attr [tag]
  (if-some [[_ tag attr value] (re-matches #"(.*)\[(.*)=(.*)\]" tag)]
    {:tag tag
     :attr attr
     :value value}
    {:tag tag}))

(defn make-tag-getter [query]
  (->> (string/split query #"\.")
       (map ->tag-attr)
       (map #(comp (tag= %) (attr= %) children))
       (apply comp)))

(defn query-select [items query]
  (let [tag-getter (make-tag-getter query)]
    (sequence tag-getter items)))
