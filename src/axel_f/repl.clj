(ns axel-f.repl)

(defn run
  "start read eval print loop"
  [ctx compile-fn]
  (println "press ctrl-D or type `exit` to exit")
  (loop [ctx ctx]
    (print "=> ")
    (flush)
    (let [formula (read-line)
          [_ _ ctx-var formula] (re-matches #"((\w+) :=)?(.*)" formula)
          [_ clj? clj-form] (re-matches #" (#clj )?(.*)" formula)]
      (if (= "exit" formula)
        (println "Buy!")
        (if clj?
          (let [res (eval (read-string clj-form))]
            (if ctx-var
              (println ctx-var "=" res)
              (println res))
            (recur (if ctx-var (assoc ctx ctx-var res) ctx)))
          (let [f (try (compile-fn formula)
                       (catch #?(:clj Throwable :cljs js/Error) _
                         (println "formula read error\n")))
                res (if f
                      (try (f ctx)
                           (catch #?(:clj Throwable :cljs js/Error) _
                             (println "formula eval error\n")
                             ::fail))
                      ::fail)]
            (when-not (= ::fail res)
              (if ctx-var
                (println ctx-var "=" res)
                (println res)))
            (recur (if ctx-var (assoc ctx ctx-var res) ctx))))))))
