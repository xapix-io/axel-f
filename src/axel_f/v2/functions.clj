(ns axel-f.v2.functions
  (:require [axel-f.v2.types :as t]))

(def ^:dynamic *registry* (atom {}))

(def default-registry
  {:ADD_OP        {:impl +'
                   :args (t/coll-of? Number)
                   :res (constantly #{Number})}
   :SUB_OP        {:impl -'
                   :args (t/coll-of? Number)
                   :res (constantly #{Number})}
   :MULT_OP       {:impl *'
                   :args (t/coll-of? Number)
                   :res (constantly #{Number})}
   :DIV_OP        {:impl /
                   :args (t/coll-of? Number)
                   :res (constantly #{Number})}
   :CONCAT_OP     {:impl str
                   :args (t/coll-of? String)
                   :res (fn [& args]
                          (t/mix-of (conj args #{String})))}
   :MORE_OP       {:impl >
                   :args (t/coll-of? Number)
                   :res (constantly #{Boolean})}
   :LESS_OP       {:impl <
                   :args (t/coll-of? Number)
                   :res (constantly #{Boolean})}
   :MORE_OR_EQ_OP {:impl >=
                   :args (t/coll-of? Number)
                   :res (constantly #{Boolean})}
   :LESS_OR_EQ_OP {:impl <=
                   :args (t/coll-of? Number)
                   :res (constantly #{Boolean})}
   :NOT_EQ_OP     {:impl not=
                   :args (t/coll-of? java.lang.Comparable)
                   :res (constantly #{Boolean})}
   :EQ_OP         {:impl =
                   :args (t/coll-of? java.lang.Comparable)
                   :res (constantly #{Boolean})}
   :NOT_OP        {:impl not
                   :args (t/coll-of? Object)
                   :res (constantly #{Boolean})}})
