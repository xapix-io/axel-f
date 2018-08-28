(ns axel-f.functions.coercion)

(defn excel-number [maybe-number]
  (cond
    (number? maybe-number)
    maybe-number

    (string? maybe-number)
    (try
      (let [n (#?(:clj read-string
                  :cljs js/parseFloat) maybe-number)]
        (when (and (number? n) #?(:cljs (not (js/isNaN n))))
          n))
      (catch #?(:clj Throwable
                :cljs js/Error) e
        nil))

    (boolean? maybe-number)
    (if maybe-number 1 0)

    :otherwise nil))

(defn excel-str [item]
  (case item
    true "TRUE"
    false "FALSE"
    (str item)))
