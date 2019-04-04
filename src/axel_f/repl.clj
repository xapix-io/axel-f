(ns axel-f.repl
  (:require [axel-f.core :as af]))

(defn run
  "start read eval print loop"
  [ctx]
  (println "press ctrl-D or type `end` to exit")
  (loop []
    (print "? ")
    (flush)
    (let [formula (read-line)]
      (if (= "end" formula)
        (print "end")
        (let [f (try (af/compile formula) (catch Throwable e (print "formula read error\n")))
              res (when f (try (f ctx) (catch Throwable e (print "formula eval error\n"))))]
          (when res (print "=" res "\n"))
          (recur))))))
