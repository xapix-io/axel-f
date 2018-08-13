(ns axel-f.core
  (:require [instaparse.core :as insta])
  (:refer-clojure :exclude [compile]))

(def parser
  (insta/parser
   "
FORMULA                  ::= EXPR | <eq-op> EXPR | <formula-begining> EXPR <formula-ending>
EXPR                     ::= COMPARISON_EXPS
COMPARISON_EXPS          ::= MORE_EXPR | LESS_EXPR | MORE_OR_EQ_EXPR | LESS_OR_EQ_EXPR | EQ_EXPR | NOT_EQ_EXPR
MORE_EXPR                ::= ( CONCAT_EXPR | COMPARISON_EXPS ) {<more-op> COMPARISON_EXPS }
LESS_EXPR                ::= ( CONCAT_EXPR | COMPARISON_EXPS ) {<less-op> COMPARISON_EXPS }
MORE_OR_EQ_EXPR          ::= ( CONCAT_EXPR | COMPARISON_EXPS ) {<more-or-eq-op> COMPARISON_EXPS }
LESS_OR_EQ_EXPR          ::= ( CONCAT_EXPR | COMPARISON_EXPS ) {<less-or-eq-op> COMPARISON_EXPS }
EQ_EXPR                  ::= ( CONCAT_EXPR | COMPARISON_EXPS ) {<eq-op> COMPARISON_EXPS }
NOT_EQ_EXPR              ::= ( CONCAT_EXPR | COMPARISON_EXPS ) {<not-eq-op> COMPARISON_EXPS }
CONCAT_EXPR              ::= ADDITIVE_EXPS {<concat-op> ADDITIVE_EXPS}
ADDITIVE_EXPS            ::= ADD_EXPR | SUB_EXPR
ADD_EXPR                 ::= ( MULTIPLICATIVE_EXPS | ADDITIVE_EXPS ) {<add-op> MULTIPLICATIVE_EXPS}
SUB_EXPR                 ::= ( MULTIPLICATIVE_EXPS | ADDITIVE_EXPS ) {<sub-op> MULTIPLICATIVE_EXPS}
MULTIPLICATIVE_EXPS      ::= MULT_EXPR | DIV_EXPR
MULT_EXPR                ::= ( EXP_EXPR | MULTIPLICATIVE_EXPS ) {<mult-op> EXP_EXPR}
DIV_EXPR                 ::= ( EXP_EXPR | MULTIPLICATIVE_EXPS ) {<div-op> EXP_EXPR}
EXP_EXPR                 ::= PRIMARY {<exp-op> PRIMARY}
PRIMARY                  ::= <whitespace> * ( <opening-parenthesis> EXPR <closing-parenthesis> | ( CONST / OBJREF ) | FNCALL | SIGN_EXPR | PERCENT_EXPR ) <whitespace> *
SIGN_EXPR                ::= ( '+' | '-' ) PRIMARY
PERCENT_EXPR             ::= PRIMARY <percent-op>
CONST                    ::= NUMBER | STRING | BOOL
NUMBER                   ::= #'[0-9]+\\.?[0-9]*(e[0-9]+)?'
STRING                   ::= #'\"[^\"]+\"'
BOOL                     ::= #'TRUE|FALSE|True|False|true|false'
FNCALL                   ::= FN <opening-parenthesis> ARGUMENTS <closing-parenthesis>
FN                       ::= #'(SUM|IF|MIN|MAX|ROUND|LEN|CONCATENATE|AVERAGE|AND|OR)'
ARGUMENTS                ::= ARGUMENT {<comma> ARGUMENT}
ARGUMENT                 ::= EXPR | Epsilon
OBJREF                   ::= FIELD (( <dot> FIELD ) | ( <opening-square-bracket> NUMBER_FIELD <closing-square-bracket> ) )*
FIELD                    ::= STRING_FIELD | SYMBOL_FIELD
STRING_FIELD             ::= STRING
SYMBOL_FIELD             ::= #'[a-zA-Z0-9-_]+'
NUMBER_FIELD             ::= #'[0-9]+'
<opening-square-bracket> ::= '['
<closing-square-bracket> ::= ']'
<formula-begining>       ::= '{='
<formula-ending>         ::= '}'
<whitespace>             ::= #'(\\s)+'
<opening-parenthesis>    ::= '('
<closing-parenthesis>    ::= ')'
<percent-op>             ::= '%'
<concat-op>              ::= '&'
<exp-op>                 ::= '^'
<more-op>                ::= '>'
<less-op>                ::= '<'
<more-or-eq-op>          ::= '>='
<less-or-eq-op>          ::= '<='
<eq-op>                  ::= '='
<not-eq-op>              ::= '<>'
<add-op>                 ::= '+'
<sub-op>                 ::= '-'
<mult-op>                ::= '*'
<div-op>                 ::= '/'
<comma>                  ::= ','
<dot>                    ::= '.'
  "))

(defn round2
  "Round a double to the given precision (number of significant digits)"
  [d precision]
  (let [factor (Math/pow 10 precision)
        res (/ (Math/round (* d factor)) factor)]
    (if (> precision 0)
      res
      (int res))))

(defn optimize-token [token]
  (fn
    ([arg] arg)
    ([arg & args]
     (vec
      (cons token
            (cons arg args))))))

(def optimize-transforms
  {:EXPR                identity
   :ARGUMENTS           vector
   :ARGUMENT            identity
   :FORMULA             identity
   :PRIMARY             identity
   :CONST               identity
   :NUMBER              read-string
   :FN                  identity
   :FIELD               identity
   :NUMBER_FIELD        read-string
   :STRING_FIELD        identity
   :SYMBOL_FIELD        identity
   :STRING              (fn [s] (apply str (-> s rest butlast)))
   :BOOL                (fn [b]
                          (case b
                            "TRUE"  true
                            "True"  true
                            "true"  true
                            "FALSE" false
                            "False" false
                            "false" false))
   :COMPARISON_EXPS     identity
   :ADDITIVE_EXPS       identity
   :MULTIPLICATIVE_EXPS identity
   :MORE_EXPR           (optimize-token :MORE_EXPR)
   :MULT_EXPR           (optimize-token :MULT_EXPR)
   :CONCAT_EXPR         (optimize-token :CONCAT_EXPR)
   :NOT_EQ_EXPR         (optimize-token :NOT_EQ_EXPR)
   :LESS_OR_EQ_EXPR     (optimize-token :LESS_OR_EQ_EXPR)
   :SUB_EXPR            (optimize-token :SUB_EXPR)
   :ADD_EXPR            (optimize-token :ADD_EXPR)
   :EXP_EXPR            (optimize-token :EXP_EXPR)
   :DIV_EXPR            (optimize-token :DIV_EXPR)
   :EQ_EXPR             (optimize-token :EQ_EXPR)
   :LESS_EXPR           (optimize-token :LESS_EXPR)
   :MORE_OR_EQ_EXPR     (optimize-token :MORE_OR_EQ_EXPR)})

(defn compile [formula-str]
  (let [res (insta/transform
             optimize-transforms
             (parser formula-str))]
    (if (insta/failure? res)
      (throw (ex-info (str "Formula \"" formula-str "\" can't be parsed.")
                      (insta/get-failure res)))
      res)))

(declare run*)

(defn run-fncall* [f args context]
  (case f
    "SUM"         (reduce +' (flatten (map #(run* % context) args)))
    "LEN"         (count (flatten (map #(run* % context) args)))
    "MIN"         (reduce min (flatten (map #(run* % context) args)))
    "MAX"         (reduce max (flatten (map #(run* % context) args)))
    "CONCATENATE" (reduce str (flatten (map #(run* % context) args)))
    "IF"          (if (run* (first args) context)
                    (run* (second args) context)
                    (when-let [else (nth args 2 nil)]
                      (run* else context)))
    "AVERAGE"     (if-let [l (not-empty (flatten (map #(run* % context) args)))]
                    (/ (reduce +' l)
                       (count l)))
    "ROUND"       (let [d (double (run* (first args) context))
                        p (second args)
                        p (if p (run* p context) 0)]
                    (round2 d p))
    "AND"         (every? identity (map #(run* % context) args))
    "OR"          (some identity (map #(run* % context) args))))

(defn run* [arg context]
  (let [token (if (vector? arg)
                (first arg)
                arg)
        args  (if (vector? arg)
                (rest arg)
                nil)]
    (cond
      (or (string? token)
          (number? token)
          (boolean? token))
      token

      (keyword? token)
      (case token
        :MORE_EXPR       (apply >
                                (map #(run* % context) args))
        :MORE_OR_EQ_EXPR (apply >=
                                (map #(run* % context) args))
        :LESS_EXPR       (apply <
                                (map #(run* % context) args))
        :LESS_OR_EQ_EXPR (apply <=
                                (map #(run* % context) args))
        :EQ_EXPR         (apply =
                                (map #(run* % context) args))
        :NOT_EQ_EXPR     (apply not=
                                (map #(run* % context) args))
        :MULT_EXPR       (apply *'
                                (map #(run* % context) args))
        :DIV_EXPR        (apply /
                                (map #(run* % context) args))
        :CONCAT_EXPR     (apply str
                                (map #(run* % context) args))
        :SUB_EXPR        (apply -'
                                (map #(run* % context) args))
        :ADD_EXPR        (apply +'
                                (map #(run* % context) args))
        :SIGN_EXPR       (let [r (run* (second args) context)]
                           (if (= (first args) "-")
                             (* -1 r)
                             r))
        :OBJREF          (get-in context (map #(run* % context) args))
        :FNCALL          (run-fncall* (first args) (second args) context)))))

(defn run
  ([formula] (run formula {}))
  ([formula context]
   (let [formula (if (string? formula)
                   (compile formula)
                   formula)]
             (run* formula context))))
