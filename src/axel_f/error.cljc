(ns axel-f.error)

(defn error [type & [reason & [data]]]
  (ex-info type
           (merge {:type type}
                  (when reason
                    {:reason reason})
                  (when data
                    {:data data}))))

(defn format-not-a-number-error [fnname arg-position value]
  (str "Function " fnname " parameter "
       arg-position
       (when arg-position " ")
       "expects number values. But '" value "' is a text and cannot be coerced to a number."))
