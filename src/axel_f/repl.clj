(ns axel-f.repl
  (:require [axel-f.core :as af]))

(defn run
  "start read eval print loop"
  [ctx]
  (println "press ctrl-D or type `exit` to exit")
  (loop []
    (print "? ")
    (flush)
    (let [formula (read-line)]
      (if (= "exit" formula)
        (println "Buy!")
        (let [f (try (af/compile formula)
                     (catch Throwable e
                       (println "formula read error\n")))
              res (if f
                    (try (f ctx)
                         (catch Throwable e
                           (println "formula eval error\n")
                           ::fail))
                    ::fail)]
          (when-not (= ::fail res)
            (println "=" res))
          (recur))))))
