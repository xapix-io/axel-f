(ns axel-f.v2.functions)

(def ^:dynamic *registry* (atom {}))

(def default-registry
  {:ADD_OP        {:impl +'}
   :SUB_OP        {:impl -'}
   :MULT_OP       {:impl *'}
   :DIV_OP        {:impl /}
   :CONCAT_OP     {:impl str}
   :MORE_OP       {:impl >}
   :LESS_OP       {:impl <}
   :MORE_OR_EQ_OP {:impl >=}
   :LESS_OR_EQ_OP {:impl <=}
   :NOT_EQ_OP     {:impl not=}
   :EQ_OP         {:impl =}
   :NOT_OP        {:impl not}})
