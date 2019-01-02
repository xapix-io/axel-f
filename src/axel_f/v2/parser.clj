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
PREFIX_EXPR = ( ADD_OP | SUB_OP ) EXPR
INFIX_EXPR = EXPR INFIX_OP EXPR ( INFIX_OP EXPR ) *
INFIX_OP = ADD_OP | SUB_OP | MULT_OP | DIV_OP | CONCAT_OP | EXP_OP | MORE_OP | LESS_OP | MORE_OR_EQ_OP | LESS_OR_EQ_OP | NOT_EQ_OP | EQ_OP
POSTFIX_EXPR = EXPR PERCENT_OP
PREFIX_EXPR = ( ADD_OP | SUB_OP | BANG_OP ) EXPR
REFERENCE_EXPR = ROOT_REFERENCE CHILD_EXPR +
ROOT_REFERENCE = GLOBAL_ROOT | LOCAL_ROOT
CHILD_EXPR = ( <'.'> ( KEYWORD / SYMBOL / STRING ) ) | ( <'.'> ? <'['> ( WILDCART | INTEGER ) <']'> )
GLOBAL_ROOT = <'$'>
LOCAL_ROOT = <'@'>
PERCENT_OP = <'%'>
ADD_OP = <'+'>
SUB_OP = <'-'>
MULT_OP = <'*'>
WILDCART = <'*'>
DIV_OP = <'/'>
CONCAT_OP = <'&'>
EXP_OP = <'^'>
BANG_OP = <'!'>
MORE_OP = <'>'>
LESS_OP = <'<'>
MORE_OR_EQ_OP = <'>='>
LESS_OR_EQ_OP = <'<='>
NOT_EQ_OP = <'<>'>
EQ_OP = <'='>
CONSTANT = STRING | NUMBER | KEYWORD | ARRAY | OBJECT
BOOLEAN = TRUE | FALSE
TRUE = <'TRUE'> | <'True'> | <'true'>
FALSE = <'FALSE'> | <'False'> | <'false'>
STRING = <'\\''> STRING_IN_SINGLE <'\\''> | <'\"'> STRING_IN_DOUBLE <'\"'>
STRING_IN_SINGLE = #'[^\\']*'
STRING_IN_DOUBLE = #'[^\"]*'
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

(def clear-tx
  {:FORMULA (fn [& exprs]
              (into [:FORMULA] exprs))

   :CONSTANT identity

   :EXPR identity

   :CHILD_EXPR identity

   :FN_EXPR (fn [fnname & args]
              (into [fnname]
                    args))

   :FN_NAME_P identity

   :FN_NAME (fn [& parts]
              ;; TODO check function
              (string/join "." parts))

   :NUMBER edn/read-string

   :STRING_IN_SINGLE identity

   :STRING_IN_DOUBLE identity

   :STRING identity

   :INTEGER edn/read-string

   :BOOLEAN #(-> % first (case :TRUE true :FALSE false))

   :SYMBOL symbol

   :NAMESPACE (fn [& symbols]
                (->> symbols
                     (map str)
                     (string/join ".")
                     symbol))

   :KEYWORD (fn
              ([s] (keyword (str s)))
              ([n s] (keyword (str n) (str s))))

   :ROOT_REFERENCE first

   :WILDCART (constantly :WILDCART)

   :POSTFIX_EXPR (fn [value _]
                   [:MULT_OP 0.01 value])

   :PREFIX_EXPR (fn [[operator] value]
                  (case operator

                    :SUB_OP
                    [:MULT_OP -1 value]

                    :BANG_OP
                    [:NOT_OP value]

                    value))

   :INFIX_OP first

   :INFIX_EXPR (fn [& forms]
                 (infix->prefix forms))

   :REFERENCE_EXPR (fn [& forms]
                     (into [:REF_OP]
                           (if (contains? #{:GLOBAL_ROOT :LOCAL_ROOT}
                                          (first forms))
                             forms
                             (cons :GLOBAL_ROOT forms))))

   :OBJECT_ENTRY (fn [k v]
                   [k v])

   :OBJECT (fn [& map-entries]
             (into [:OBJECT_OP]
                   map-entries))

   :ARRAY (fn [& elements]
            (into [:ARRAY_OP]
                  elements))})

(defn parse [formula]
  (let [tokens (insta/parse parser formula)]
    (if-let [fail (insta/get-failure tokens)]
      (throw (ex-info (with-out-str (failure/pprint-failure (insta/get-failure tokens)))
                      fail))
      (insta/transform clear-tx tokens))))

(comment

  (let [x (exec-fn (parse "$.:axel-f.parser/foo.bar.'baz'.[*]"))]
    (time
     (x {::foo {'bar {"baz" 1}}})))

  ((exec-fn (parse "'qwe' & $.'foo'")) {"foo" "qwe"})

  (time (not= (> 1 2 0) true))

  (let [x (exec-fn (parse "!!(1 + $.:axel-f.parser/foo.:axel-f.parser/bar)"))]
    (x {::foo {::bar 1}}))

  (time
   (+ 1 (get-in {::foo {'bar {"baz" 1}}} [::foo 'bar "baz"])))

  (type (-> (parse "foo") second last))

  (insta/parse parser "$.:foo.:bar")
  (-> (parse "$.:foo.:bar") second (nth 2) type)

  (let [tf (axel-f.v2.type-system/type-fn (parse "'Hello, ' & $.:user.:name & '. Your password is ' & $.:user.:password"))]
    (tf {:user {:name ""
                :password #{String ::secure}}}))

  (let [tf (axel-f.v2.type-system/type-fn (parse "2 + $.:user.:name + 1 + $.:user.:password"))]
    (tf {:user {:name "qwe"
                :password #{Number ::secure}}}))

  )
