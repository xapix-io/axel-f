(ns axel-f.v2.parser
  (:require [instaparse.core :as insta :refer [defparser]]
            [instaparse.failure :as failure]
            [clojure.string :as string]
            [clojure.edn :as edn]))

(defparser parser
  "
FORMULA = <nl> * ( ASSIGN_EXPR <nl> * ) *  EXPR <nl> *
ASSIGN_EXPR = REFERENCE_EXPR <ws> <'='> EXPR
EXPR = <ws> * EXPR <ws> * / <'('> EXPR <')'> / FN_EXPR / PREFIX_EXPR / INFIX_EXPR / POSTFIX_EXPR / BOOLEAN / REFERENCE_EXPR / CONSTANT
FN_EXPR = ( FN_NAME <'()'> ) | ( FN_NAME <'('> EXPR ( <','> EXPR ) * ( <','> <ws> * ) ? <')'> )
FN_NAME = FN_NAME_P ( <'.'> FN_NAME_P ) *
FN_NAME_P = #'[A-Z][A-Z0-9]+'
PREFIX_EXPR = PREFIX_OP EXPR
PREFIX_OP = '+' | '-' | '!'
INFIX_EXPR = EXPR INFIX_OP EXPR ( INFIX_OP EXPR ) *
INFIX_OP = '+' | '-' | '*' | '/' | '&' | '^' | '>' | '<' | '>=' | '<=' | '<>' | '='
POSTFIX_EXPR = EXPR POSTFIX_OP
POSTFIX_OP = '%'
REFERENCE_EXPR = ROOT_REFERENCE CHILD_EXPR +
ROOT_REFERENCE = '$' | '@'
CHILD_EXPR = ( <'.'> ( KEYWORD / SYMBOL / STRING ) ) | ( <'.'> ? <'['> ( WILDCART | INTEGER ) <']'> )
WILDCART = <'*'>
CONSTANT = STRING | NUMBER | KEYWORD | BOOLEAN | ARRAY | OBJECT
BOOLEAN = 'TRUE' | 'True' | 'true' | 'FALSE' | 'False' | 'false'
STRING = <'\\''> #'[^\\']*' <'\\''> | <'\"'> #'[^\"]*' <'\"'>
KEYWORD = <':'> ( NAMESPACE <'/'> ) ? SYMBOL ( <'.'> SYMBOL ) *
NUMBER = #'[0-9]+\\.?[0-9]*(e[0-9]+)?'
INTEGER = #'[0-9]*'
NAMESPACE = SYMBOL ( <'.'> SYMBOL ) *
SYMBOL = #'[^0-9\\'\"!. ][a-zA-Z0-9*+!_\\'?<>-]+'
ARRAY = <'['> EXPR ? ( <','> EXPR ) * <']'>
OBJECT = <'{'> OBJECT_ENTRY ? ( <','> <ws> * OBJECT_ENTRY ) * <'}'>
OBJECT_ENTRY = CONSTANT <ws> * <':'> EXPR
<ws> = #'(\\s)+'
<nl> = #'(\\n)+'
")

(def next-operator
  "Infix operator precedence"
  {:MULT_OP :EXP_OP
   :DIV_OP :MULT_OP
   :ADD_OP :DIV_OP
   :SUB_OP :ADD_OP
   :MORE_OP :SUB_OP
   :LESS_OP :MORE_OP
   :MORE_OR_EQ_OP :LESS_OP
   :LESS_OR_EQ_OP :MORE_OR_EQ_OP
   :NOT_EQ_OP :LESS_OR_EQ_OP
   :EQ_OP :NOT_EQ_OP
   :CONCAT_OP :EQ_OP})

(defn infix->prefix
  ([forms]
   (infix->prefix forms :CONCAT_OP))
  ([forms op]
   (if (and op (not= 1 (count forms)))
     (let [forms' (partition-by (partial = op) forms)
           op' (next-operator op)]
       (if (= 1 (count forms'))
         (if-let [op' (next-operator op)]
           (infix->prefix forms op')
           (first forms))
         (into [op]
               (map #(infix->prefix % op')
                    (filter #(not= (list op) %)
                            forms')))))
     (first forms))))

(defn operator->token [op]
  (case op
    "+" :ADD_OP
    "-" :SUB_OP
    "*" :MULT_OP
    "/" :DIV_OP
    "<" :LESS_OP
    ">" :MORE_OP
    "<=" :LESS_OR_EQ_OP
    ">=" :MORE_OR_EQ_OP
    "<>" :NOT_EQ_OP
    "=" :EQ_OP
    "!" :BANG_OP
    "&" :CONCAT_OP
    "^" :EXP_OP
    "%" :PERCENT_OP
    "$" :GLOBAL_ROOT
    "@" :LOCAL_ROOT))

(defn wrap-formula [& exps]
  (into [:FORMULA] exps))

(defn wrap-fn [fn-name & args]
  (into [fn-name] args))

(defn fn-name->token [& fn-parts]
  (string/join "." fn-parts))

(defn wrap-boolean [b]
  (case b
    "TRUE" true
    "True" true
    "true" true
    "FALSE" false
    "False" false
    "false" false))

(defn wrap-namespace [& symbols]
  (->> symbols
       (map str)
       (string/join ".")
       symbol))

(defn wrap-keyword
  ([s] (keyword (str s)))
  ([n s] (keyword (str n) (str s))))

(defn wrap-prefix-expr [operator value]
  (case operator
    :SUB_OP  [:MULT_OP -1 value]
    :BANG_OP [:NOT_OP value]
    value))

(defn wrap-infix-expr [& forms]
  (infix->prefix forms))

(defn wrap-postfix-expr [value _]
  ;; Atm only percent operator supported
  [:MULT_OP 0.01 value])

(defn wrap-reference-expr [& forms]
  (into [:REF_OP] forms))

(def clear-tx
  {:FORMULA          wrap-formula
   :CONSTANT         identity
   :EXPR             identity
   :CHILD_EXPR       identity
   :FN_EXPR          wrap-fn
   :FN_NAME_P        identity
   :FN_NAME          fn-name->token
   :NUMBER           edn/read-string
   :STRING           identity
   :INTEGER          edn/read-string
   :BOOLEAN          wrap-boolean
   :SYMBOL           symbol
   :NAMESPACE        wrap-namespace
   :KEYWORD          wrap-keyword
   :ROOT_REFERENCE   operator->token
   :WILDCART         (constantly :WILDCART)
   :PREFIX_OP        operator->token
   :INFIX_OP         operator->token
   :POSTFIX_OP       operator->token
   :PREFIX_EXPR      wrap-prefix-expr
   :INFIX_EXPR       wrap-infix-expr
   :POSTFIX_EXPR     wrap-postfix-expr
   :REFERENCE_EXPR   wrap-reference-expr
   :OBJECT_ENTRY     vector
   :OBJECT           (partial into [:OBJECT_OP])
   :ARRAY            (partial into [:ARRAY_OP])})

(defn parse [formula]
  (let [tokens (insta/parse parser formula)]
    (if-let [fail (insta/get-failure tokens)]
      (throw (ex-info (with-out-str (failure/pprint-failure (insta/get-failure tokens)))
                      fail))
      (insta/transform clear-tx tokens))))
