(ns axel-f.error)

(defn error [type & [reason & [data]]]
  (ex-info type
           (merge {:type type}
                  (when reason
                    {:reason reason})
                  (when data
                    {:data data}))))

(defn error? [maybe-error]
  (and (map? maybe-error)
       (:error maybe-error)))
