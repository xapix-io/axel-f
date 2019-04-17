(ns axel-f.excel.logic)

(defn AND*
  "Returns true if all of the provided arguments are logically true, and false if any of the provided arguments are logically false."
  [& args]
  (every? identity args))

(def AND #'AND*)

(defn NOT*
  "Returns the opposite of a logical value - `NOT(TRUE)` returns `FALSE`; `NOT(FALSE)` returns `TRUE`."
  [logical-expression]
  (not logical-expression))

(def NOT #'NOT*)

(defn OR*
  "Returns true if any of the provided arguments are logically true, and false if all of the provided arguments are logically false."
  [& args]
  (boolean (some identity args)))

(def OR #'OR*)

(def env
  {"AND" AND
   "OR" OR
   "NOT" NOT})
