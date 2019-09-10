(ns axel-f.excel.validate)

(defn presence*
  "Validate and return given value if not null, and throw an exception with given message (default is 'Argument is required')."
  [^{:doc "Object to check for presence."} x
   & [^{:doc "Optional message to be thrown."} msg]]
  (let [msg (or msg "Argument required")]
    (if (some? x) x (throw (ex-info msg {:type ::validate-error
                                         :subtype ::presence})))))

(def presence #'presence*)

(defn not-empty**
  "Validate and return given value if not empty collection, and throw an exception with given message (default is 'Argument can not be empty'). "
  [^{:doc "Object to check for emptiness."} x
   & [^{:doc "Optional message to be thrown."} msg]]
  (let [msg (or msg "Argument can not be empty")]
    (or (if ((some-fn coll? string?) x)
          (not-empty x)
          x)
        (throw (ex-info msg {:type ::validate-error
                             :subtype ::empty})))))

(def not-empty* #'not-empty**)

(def env
  {"validate" {"presence" presence
               "not-empty" not-empty*}})
