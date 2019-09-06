(ns axel-f.excel.fn)

(defn composition*
  "Takes a set of functions and returns a fn that is the composition of those fns. The returned fn takes a variable number of args, applies the rightmost of fns to the args, the next fn (right-to-left) to the result, etc."
  [& ^{:doc "Function to compose"} fns]
  (apply comp fns))

(def composition #'composition*)

(def env
  {"composition" composition})
