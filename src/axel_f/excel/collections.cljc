(ns axel-f.excel.collections
  (:require [axel-f.excel.utils :as ut]
            [clojure.string :as string]))

(defn MAP*
  "Applies partialy defined formula to every element in a collection and returns an array."
  [^{:doc "Partialy defined formula with free variables to apply to the collection"} f
   & ^{:doc "Collection of elements"} colls]
  (apply map f colls))

(def MAP #'MAP*)

(defn FILTER*
  "Returns an array of elements that have been filtered based on a condition."
  [^{:doc "Condition predicate which will be applied to members of collection"} pred
   ^{:doc "Collection of elements"} coll]
  (filter pred coll))

(def FILTER #'FILTER*)

(defn SORT*
  "Sorts a collection by the values returned from applying a sorting function to each element in said collection."
  [^{:doc "Sorting function that will be applied to each element of the collection"} keyfn
   ^{:doc "Collection of elements"} coll]
  (sort-by keyfn coll))

(def SORT #'SORT*)

(defn CONCAT*
  "Concatenates arrays"
  [& ^{:doc "Arrays to concatenate"} colls]
  (apply concat colls))

(def CONCAT #'CONCAT*)

(defn- walk
  ([f data] (walk f -1 data))
  ([f level data]
   (cond
     (= 0 level) (f data)
     (sequential? data) (map (partial walk f (dec level)) data)
     (map? data) (ut/map-vals (partial walk f (dec level)) data)
     :else (f data))))

(def children (mapcat #(get % "children")))

(defn tag= [tag]
  (comp (filter #(= (:tag tag)
                    (get % "tag")))
        children))

(defn extract-attr-getter [tag]
  (if-some [[_ tag attr value] (re-matches #"(.*)\[(.*)=(.*)\]" tag)]
    {:tag tag
     :attr attr
     :value value}
    {:tag tag}))

(defn make-tag-getter [query]
  (->> (string/split query #"\.")
       (map extract-attr-getter)
       (map tag=)
       (apply comp)))

(defn query-select [items query]
  (let [tag-getter (make-tag-getter query)]
    (sequence tag-getter items)))

(comment
  (let [_ "<?xml version=\"1.0\" encoding=\"UTF-8\"?><configuration>\n\n  <appender name=\"STDOUT\" class=\"ch.qos.logback.core.ConsoleAppender\">\n    <!-- encoders are assigned the type\n         ch.qos.logback.classic.encoder.PatternLayoutEncoder by default -->\n    <encoder>\n      <pattern>%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n</pattern>\n    </encoder>\n  </appender>\n\n  <root level=\"debug\">\n    <appender-ref ref=\"STDOUT\"/>\n  </root>\n</configuration>"
      _ "<?xml version= \"1.0\"?><response><id-number>2716902077</id-number><summary-result><key>id.success</key><message>PASS</message></summary-result><results><key>result.match</key><message>ID Located</message></results></response>"
      parsed {"tag" "configuration", "children" [{"tag" "appender", "children" [{"children" [" encoders are assigned the type\n         ch.qos.logback.classic.encoder.PatternLayoutEncoder by default "]} {"tag" "encoder", "children" [{"tag" "pattern", "children" ["%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n"]}]}], "attrs" {"name" "STDOUT", "class" "ch.qos.logback.core.ConsoleAppender"}} {"tag" "root", "children" [{"tag" "appender-ref", "attrs" {"ref" "STDOUT"}}], "attrs" {"level" "debug"}} {"tag" "something", "children" ["halo"]}]}
      parsed2 {"tag" "response", "children" [{"tag" "id-number", "children" ["2716902077\n  "]} {"tag" "summary-result", "children" [{"tag" "key", "children" ["id.success\n    "]} {"tag" "message", "children" ["PASS"]} {"tag" "message", "children" ["PASS22"]}]} {"tag" "results", "children" [{"tag" "key", "children" ["result.match\n    "]} {"tag" "message", "children" ["ID Located\n    "]}]}]}
      items [parsed2]
      query "response.summary-result.message"]
  (query-select items query))
  )

(def env
  {"MAP"    MAP
   "KEEP"   FILTER
   "CONCAT" CONCAT
   "SORT"   SORT
   "walk"   walk})
