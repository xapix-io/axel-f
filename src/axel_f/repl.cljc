(ns axel-f.repl
  (:require [clojure.string :as string]))

(defn run
  "start read eval print loop"
  [ctx eval-fn]
  (println "press ctrl-D or type `exit` to exit")
  (loop [ctx ctx]
    (print "=> ")
    (flush)
    (let [formula (read-line)
          [_ _ ctx-var formula] (re-matches #"((\w+) :=)?(.*)" formula)]
      (if (= "exit" formula)
        (println "Buy!")
        (let [f (try (eval-fn formula)
                     (catch #?(:clj Throwable :cljs js/Error) e
                       (println "formula read error\n")))
              res (if f
                    (try (f ctx)
                         (catch #?(:clj Throwable :cljs js/Error) e
                           (println "formula eval error\n")
                           ::fail))
                    ::fail)]
          (when-not (= ::fail res)
            (if ctx-var
              (println ctx-var "=" res)
              (println res)))
          (recur (if ctx-var (assoc ctx ctx-var res) ctx)))))))
