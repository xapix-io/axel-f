(ns axel-f.excel.special-forms)

(def env
  {"IF" (with-meta (fn [])
          {:doc "Evaluates test. If not the singular values nil or false, evaluates and yields then, otherwise, evaluates and yields else. If else is not supplied it defaults to nil."
           :arglists '([^{:doc "Test expression."} test
                        ^{:doc "Then expression. Eval and yield if test expression is not `null` or `false`"} then
                        & [^{:doc "Else expression."} else]])})
   "IFS" (with-meta (fn [])
           {:doc "Takes a set of test/expr pairs. It evaluates each test one at a time.  If a test returns logical true, cond evaluates and returns the value of the corresponding expr and doesn't evaluate any of the other tests or exprs. (IFS) returns nil."
            :arglists '([& ^{:doc "Test expression."} test
                         & ^{:doc "Then expression."} then
                         & [^{:doc "Else expression."} else]])})
   "WITH" (with-meta (fn [])
            {:doc "Evaluates the exprs in a lexical context in which the symbols in the binding-forms are bound to their respective init-exprs or parts therein."
             :arglists '([& ^{:doc "Local variable name."} var-name
                          & ^{:doc "Binding value"} var-value
                          ^{:doc "Expression."} body-expr])})
   "FN" (with-meta (fn [])
          {:doc "Defines a function"
           :arglists '([& ^{:doc "Variable name."} var-name
                        ^{:doc "Body expression."} body-expr])})})
