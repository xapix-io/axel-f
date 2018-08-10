(ns axel-f.core
  (:require [instaparse.core :as insta]))

(def parser
  (insta/parser
   (str
    "
FORMULA ::= EXPR | <eq-op> EXPR | <formula-begining> EXPR <formula-ending>
EXPR ::= MORE_EXPR | LESS_EXPR | MORE_OR_EQ_EXPR | LESS_OR_EQ_EXPR | EQ_EXPR | NOT_EQ_EXPR
MORE_EXPR ::= CONCAT_EXPR {<more-op> CONCAT_EXPR}
LESS_EXPR ::= CONCAT_EXPR {<less-op> CONCAT_EXPR}
MORE_OR_EQ_EXPR ::= CONCAT_EXPR {<more-or-eq-op> CONCAT_EXPR}
LESS_OR_EQ_EXPR ::= CONCAT_EXPR {<less-or-eq-op> CONCAT_EXPR}
EQ_EXPR ::= CONCAT_EXPR {<eq-op> CONCAT_EXPR}
NOT_EQ_EXPR ::= CONCAT_EXPR {<not-eq-op> CONCAT_EXPR}
CONCAT_EXPR ::= ( ADD_EXPR | SUB_EXPR ) {<concat-op> ( ADD_EXPR | SUB_EXPR )}
ADD_EXPR ::= ( MULT_EXPR | DIV_EXPR ) {<add-op> ( MULT_EXPR | DIV_EXPR )}
SUB_EXPR ::= ( MULT_EXPR | DIV_EXPR ) {<sub-op> ( MULT_EXPR | DIV_EXPR )}
MULT_EXPR ::= EXP_EXPR {<mult-op> ( EXP_EXPR | MULT_EXPR | DIV_EXPR )}
DIV_EXPR ::= EXP_EXPR {<div-op> ( EXP_EXPR | MULT_EXPR | DIV_EXPR )}
EXP_EXPR ::= PRIMARY {<exp-op> PRIMARY}
PRIMARY ::= <whitespace> * ( <opening-parenthesis> EXPR <closing-parenthesis> | ( CONST / OBJREF ) | FNCALL | SIGN_EXPR | PERCENT_EXPR ) <whitespace> *
SIGN_EXPR ::= ( '+' | '-' ) PRIMARY
PERCENT_EXPR ::= PRIMARY <percent-operator>
CONST ::= NUMBER | STRING | BOOL
NUMBER ::= #'[0-9]+\\.?[0-9]*(e[0-9]+)?'
STRING ::= #'\"[^\"]+\"'
BOOL ::= #'TRUE|FALSE'
FNCALL ::= FN <opening-parenthesis> ARGUMENTS <closing-parenthesis>
FN ::= #'(SUM|SUMIF|IF|MIN|MAX|ROUND|LEN|CONCATENATE)'
ARGUMENTS ::= ARGUMENT {<comma> ARGUMENT}
ARGUMENT ::= EXPR | Epsilon
OBJREF ::= FIELD {<dot> FIELD}
FIELD ::= #'[a-zA-Z_]+[a-zA-Z0-9_]*'
<formula-begining> ::= '{='
<formula-ending> ::= '}'
<whitespace> ::= #'(\\s)+'
<concat-op> ::= '&'
<exp-op> ::= '^'
<percent-operator> ::= '%'
<opening-parenthesis> ::= '('
<closing-parenthesis> ::= ')'
<more-op> ::= '>'
<less-op> ::= '<'
<more-or-eq-op> ::= '>='
<less-or-eq-op> ::= '<='
<eq-op> ::= '='
<not-eq-op> ::= '<>'
<add-op> ::= '+'
<sub-op> ::= '-'
<mult-op> ::= '*'
<div-op> ::= '/'
<comma> ::= ','
<dot> ::= '.'
  ")))

(defn transforms [context]
  {:MORE_EXPR (fn
                ([arg] arg)
                ([arg & args]
                 (apply > arg args)))
   :MULT_EXPR (fn
                ([arg] arg)
                ([arg & args]
                 (apply * arg args)))
   :CONCAT_EXPR (fn
                  ([arg] arg)
                  ([arg & args]
                   (apply str arg args)))
   :BOOL (fn [b]
           (case b
             "TRUE" true
             "True" true
             "true" true
             "FALSE" false
             "False" false
             "false" false))
   :NOT_EQ_EXPR (fn
                  ([arg] arg)
                  ([arg & args]
                   (apply not= arg args)))
   :NUMBER read-string
   :FNCALL (fn [f args]
             (case f
               "SUM" (apply +' args)
               "IF" (if (first args)
                      (second args)
                      (nth args 2 nil))
               "MIN" (apply min args)
               "MAX" (apply max args)
               "ROUND" (with-precision (second args) (first args))
               "LEN" (count (flatten args))
               "CONCATENATE" (apply str args)))
   :SIGN_EXPR (fn [sign n]
                (if (= sign "-")
                  (* -1 n)
                  n))
   :LESS_OR_EQ_EXPR (fn
                      ([arg] arg)
                      ([arg & args]
                       (apply <= arg args)))
   :SUB_EXPR (fn
               ([arg] arg)
               ([arg & args]
                (apply - arg args)))
   :ADD_EXPR (fn
               ([arg] arg)
               ([arg & args]
                (apply + arg args)))
   :CONST identity
   :FN identity
   :EXP_EXPR (fn
               ([arg] arg)
               ([base power]
                (Math/pow base power)))
   :DIV_EXPR (fn
               ([arg] arg)
               ([arg & args]
                (apply / arg args)))
   :ARGUMENT identity
   :STRING (fn [s]
             (apply str
                    (-> s
                        rest
                        butlast)))
   :PRIMARY identity
   :FIELD identity
   :EQ_EXPR (fn
              ([arg] arg)
              ([arg & args]
               (apply = arg args)))
   :ARGUMENTS vector
   :FORMULA identity
   :LESS_EXPR (fn
                ([arg] arg)
                ([arg & args]
                 (apply < arg args)))
   :EXPR identity
   :OBJREF (fn [& fields]
             (get-in context fields))
   :MORE_OR_EQ_EXPR (fn
                      ([arg] arg)
                      ([arg & args]
                       (apply >= arg args)))})

(defn run [formula context]
  (insta/transform
   (transforms context)
   (parser formula)))

(comment
  (time
   (run "SUM(1,2,3) / LEN(foo.bar) * IF(foo.baz > foo.buz, -1, 1)"
     {"foo" {"bar" [5 6 7]
             "baz" 10
             "buz" 9}}
     ))

  (require '[criterium.core :as b])

  (b/bench
   (run "SUM(1,2,3) / LEN(foo.bar) * IF(foo.baz > foo.buz, -1, 1)"
     {"foo" {"bar" [5 6 7]
             "baz" 10
             "buz" 9}}
     )
   )

  )
