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
  (let [tag-getter (make-tag-getter query)]
    (sequence tag-getter items)))

(let [_ "<?xml version=\"1.0\" encoding=\"UTF-8\"?><configuration>\n\n  <appender name=\"STDOUT\" class=\"ch.qos.logback.core.ConsoleAppender\">\n    <!-- encoders are assigned the type\n         ch.qos.logback.classic.encoder.PatternLayoutEncoder by default -->\n    <encoder>\n      <pattern>%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n</pattern>\n    </encoder>\n  </appender>\n\n  <root level=\"debug\">\n    <appender-ref ref=\"STDOUT\"/>\n  </root>\n</configuration>"
      _ "<?xml version= \"1.0\"?><response><id-number>2716902077</id-number><summary-result><key>id.success</key><message>PASS</message></summary-result><results><key>result.match</key><message>ID Located</message></results></response>"
      parsed {"tag" "configuration", "children" [{"tag" "appender", "children" [{"children" [" encoders are assigned the type\n         ch.qos.logback.classic.encoder.PatternLayoutEncoder by default "]} {"tag" "encoder", "children" [{"tag" "pattern", "children" ["%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n"]}]}], "attrs" {"name" "STDOUT", "class" "ch.qos.logback.core.ConsoleAppender"}} {"tag" "root", "children" [{"tag" "appender-ref", "attrs" {"ref" "STDOUT"}}], "attrs" {"level" "debug"}} {"tag" "something", "children" ["halo"]}]}
      parsed2 {"tag" "response","attrs" {"foo" "bar"} "children" [{"tag" "id-number", "children" ["2716902077\n  "]} {"tag" "summary-result", "children" [{"tag" "key", "children" ["id.success\n    "]} {"tag" "message", "children" ["PASS"]} {"tag" "message", "children" ["PASS22"]}]} {"tag" "results", "children" [{"tag" "key", "children" ["result.match\n    "]} {"tag" "message", "children" ["ID Located\n    "]}]}]}
      items [parsed2]
      query "response[foo=bar].summary-result.message"]
  (query-select items query))
