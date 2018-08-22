(ns axel-f.core
  (:require #?(:clj [instaparse.core :as insta :refer [defparser]]
               :cljs [instaparse.core :as insta :refer-macros [defparser]])
            [clojure.string :as string]
            [axel-f.error :as error]
            [axel-f.functions :as functions])
  (:refer-clojure :exclude [compile]))

(defn- strings->rule [strings]
  (->> strings
       (map #(str "'" % "'"))
       (string/join " | ")))

(defparser parser
  (str
   "
FORMULA                  ::= EXPR | <eq-op> EXPR
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
PRIMARY                  ::= <whitespace> * ( <opening-parenthesis> EXPR <closing-parenthesis> | ( CONST / OBJREF ) | FNCALL | SIGN_EXPR | PERCENT_EXPR | ARRAY_EXPR | ERROR ) <whitespace> *
SIGN_EXPR                ::= ( '+' | '-' ) PRIMARY
PERCENT_EXPR             ::= PRIMARY <percent-op>
ARRAY_EXPR               ::= <opening-curly-bracket> ( EXPR {<comma> EXPR} )? <closing-curly-bracket>
ERROR                    ::= #'#N/A|#VALUE!|#REF!|#DIV/0!|#NUM!|#NAME?|#NULL!'
CONST                    ::= NUMBER | STRING | BOOL
NUMBER                   ::= #'[0-9]+\\.?[0-9]*(e[0-9]+)?'
STRING                   ::= #'\"[^\"]+\"'
BOOL                     ::= #'TRUE|FALSE|True|False|true|false'
FNCALL                   ::= FN <opening-parenthesis> ARGUMENTS <closing-parenthesis>
FN                       ::= " (-> functions/functions-map
                                   keys
                                   (conj "IF" "OBJREF")
                                   strings->rule) "
ARGUMENTS                ::= ARGUMENT {<comma> ARGUMENT}
ARGUMENT                 ::= EXPR | Epsilon
OBJREF                   ::= FIELD (( <dot> FIELD ) | ( <dot>? <opening-square-bracket> ( NUMBER_FIELD | FNCALL | STAR ) <closing-square-bracket> ) )*
FIELD                    ::= STRING_FIELD | SYMBOL_FIELD | FNCALL
STRING_FIELD             ::= STRING
SYMBOL_FIELD             ::= #'[a-zA-Z0-9-_]+'
NUMBER_FIELD             ::= #'[0-9]+'
STAR                     ::= '*'?
<opening-square-bracket> ::= '['
<closing-square-bracket> ::= ']'
<opening-curly-bracket>  ::= '{'
<closing-curly-bracket>  ::= '}'
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


(defn- with-indifferent-access [m ks]
  (if (= "*" (first ks))
    (if (and (seqable? m)
             (not= (count ks) 1))
      (mapv #(with-indifferent-access % (rest ks))
            m)
      m)
    (when (and (seqable? m)
               (not-empty m))
      (let [k (first ks)
            res (cond
                  (number? k)  (let [r (when (integer? k)
                                         (nth m k nil))]
                                 (if (nil? r)
                                   (let [r (get m k)]
                                     (if (nil? r)
                                       (let [k (str k)
                                             r (get m (str k))]
                                         (if (nil? r)
                                           (get m (keyword k))))
                                       r))
                                   r))
                  (string? k)  (let [r (get m k)]
                                 (if (nil? r)
                                   (get m (keyword k))
                                   r))
                  (keyword? k) (let [r (get m k)]
                                 (if (nil? r)
                                   (get m (name k))
                                   r)))]
        (if-let [ks (not-empty (rest ks))]
          (recur res ks)
          res)))))

(def reserved-tokens
  (set [:MORE_EXPR
        :MORE_OR_EQ_EXPR
        :LESS_EXPR
        :LESS_OR_EQ_EXPR
        :EQ_EXPR
        :NOT_EQ_EXPR
        :MULT_EXPR
        :DIV_EXPR
        :CONCAT_EXPR
        :SUB_EXPR
        :ADD_EXPR
        :SIGN_EXPR
        :PERCENT_EXPR
        :OBJREF
        :VECTOR
        :FNCALL
        :EXP_EXPR]))

(defn- reserved? [token]
  (let [token (if (string? token)
                (keyword token)
                token)]
    (boolean (reserved-tokens token))))

(defn- optimize-token [token]
  (fn
    ([arg] arg)
    ([arg & args]
     (vec
      (cons token
            (cons arg args))))))

(def ^:private optimize-transforms
  {:EXPR                identity
   :ARGUMENTS           (fn [& args]
                          (vec (remove #(= % :empty) args)))
   :ARGUMENT            (fn
                          ([] :empty)
                          ([arg] arg))
   :FORMULA             identity
   :PRIMARY             identity
   :CONST               identity
   :NUMBER              #?(:clj read-string
                           :cljs js/parseFloat)
   :FN                  identity
   :FIELD               identity
   :NUMBER_FIELD        #?(:clj read-string
                           :cljs js/parseFloat)
   :STRING_FIELD        identity
   :SYMBOL_FIELD        identity
   :STRING              (fn [s]
                          (let [s (apply str (-> s rest butlast))]
                            (if (reserved? s)
                              (throw (#?(:clj Exception.
                                         :cljs js/Error.) (str "String " s " is reserved.")))
                              s)))
   :BOOL                (fn [b]
                          (let [b (string/lower-case b)]
                            (cond
                              (= b "true") true
                              (= b "false") false)))
   :STAR                (constantly "*")
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
   :SIGN_EXPR           (fn [& args]
                          (let [operand (second args)
                                sign    (if (= "-" (first args)) -1 1)]
                            (cond
                              (boolean? operand)         (* sign (if operand 1 0))
                              (number? operand)          (* sign operand)
                              (keyword? (first operand)) (vec (cons :SIGN_EXPR args))
                              :otherwise (throw (#?(:clj Exception.
                                                    :cljs js/Error.)
                                                 (str "The operator “" (first args) "” expects a number or boolean but found " operand ".")))
                              )))
   :PERCENT_EXPR        (fn [arg]
                          (if (number? arg)
                            (float (/ arg 100))
                            [:PERCENT_EXPR arg]))
   :MORE_OR_EQ_EXPR     (optimize-token :MORE_OR_EQ_EXPR)
   :ARRAY_EXPR          (fn [& args]
                          (vec (cons :VECTOR args)))
   :ERROR               error/error})

(defn compile [formula-str & custom-transforms]
  (let [custom-transforms (into {} (map (fn [[k v]]
                                          [k v])
                                        (partition-all 2 custom-transforms)))
        res (insta/transform
             (merge optimize-transforms
                    custom-transforms)
             (parser formula-str))]
    (if (insta/failure? res)
      (throw (ex-info (str "Formula \"" formula-str "\" can't be parsed.")
                      (insta/get-failure res)))
      res)))

(declare run*)

(defn- run-special
  "Run fn which requires special/custom args evaluation"
  [f args context]
  (case f
    "OBJREF"   (with-indifferent-access context (map #(run* % context) args))
    "IF"       (if (run* (first args) context)
                 (run* (second args) context)
                 (when-let [else (nth args 2 nil)]
                   (run* else context)))))

(defn- special? [f]
  (or (= f "IF")
      (= f "OBJREF")))

(defn- apply-flatten-args [f args]
  (apply f (flatten args)))


(defn- run-fncall* [f args context]
  (if (special? f)
   (run-special f args context)
   (if-let [f-implementation (get functions/functions-map f)]
     (->> args
          (map #(run* % context))
          (apply-flatten-args f-implementation))
     ;; TODO: emit parse error
     (str "No such function: " f ))))

(defmulti ->keyword (fn [v] (type v)))

(defmethod ->keyword #?(:clj String
                        :cljs js/String) [v] (keyword v))

(defmethod ->keyword #?(:clj clojure.lang.Keyword
                        :cljs cljs.core/Keyword) [v] v)

(defn- run* [arg context]
  (let [token (if (vector? arg)
                (first arg)
                arg)
        args  (if (vector? arg)
                (rest arg)
                nil)]
    (cond
      (reserved? token)
      (case (->keyword token)
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
        :MULT_EXPR       (apply #?(:clj *' :cljs *)
                                (map #(run* % context) args))
        :DIV_EXPR        (apply /
                                (map #(run* % context) args))
        :CONCAT_EXPR     (apply str
                                (map #(run* % context) args))
        :SUB_EXPR        (apply #?(:clj -' :cljs -)
                                (map #(run* % context) args))
        :ADD_EXPR        (apply #?(:clj +' :cljs +)
                                (map #(run* % context) args))
        :SIGN_EXPR       (let [r (run* (second args) context)
                               r (if (boolean? r)
                                   (if r 1 0)
                                   r)]
                           (if (= (first args) "-")
                             (* -1 r)
                             r))
        :EXP_EXPR        (let [f (run* (first args) context)
                               s (run* (second args) context)
                               res (Math/pow f s)]
                           (if (and (integer? f) (integer? s))
                             (int res)
                             res))
        :PERCENT_EXPR    (let [r (run* (first args) context)]
                           (float (/ r 100)))
        :OBJREF          (with-indifferent-access context (map #(run* % context) args))
        :VECTOR          (mapv #(run* % context) args)
        :FNCALL          (run-fncall* (first args) (second args) context))

      (or (string? token)
          (number? token)
          (boolean? token)
          (keyword? token))
      token)))

(defn run
  ([formula] (run formula {}))
  ([formula context]
   (let [formula (if (string? formula)
                   (compile formula)
                   formula)]
     (run* formula context))))
