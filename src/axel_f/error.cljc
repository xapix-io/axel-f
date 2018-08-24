(ns axel-f.error)

(defn error [type & [reason]]
  (ex-info type
           (merge {:type type}
                  (when reason
                    {:reason reason}))))

(defn error? [maybe-error]
  (and (map? maybe-error)
       (:error maybe-error)))
