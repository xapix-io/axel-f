(ns axel-f.core
  (:require #?(:clj [instaparse.core :as insta :refer [defparser]]
               :cljs [instaparse.core :as insta :refer-macros [defparser]])
            [clojure.string :as string]
            [axel-f.error :as error]
            #?(:clj [axel-f.macros :refer [def-excel-fn find-impl]]
               :cljs [axel-f.macros :refer [find-impl] :refer-macros [def-excel-fn]]))
  (:refer-clojure :exclude [compile]))

(def parser-str
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
  PRIMARY                  ::= <whitespace> * ( <opening-parenthesis> EXPR <closing-parenthesis> | ( CONST / OBJREF ) | FNCALL | SIGN_EXPR | PERCENT_EXPR | ARRAY_EXPR | ERROR | DYNAMIC_REF ) <whitespace> *
  SIGN_EXPR                ::= ( '+' | '-' ) PRIMARY
  PERCENT_EXPR             ::= PRIMARY <percent-op>
  ARRAY_EXPR               ::= <opening-curly-bracket> ( EXPR {<comma> EXPR} )? <closing-curly-bracket>
  ERROR                    ::= #'#N/A|#VALUE!|#REF!|#DIV/0!|#NUM!|#NAME?|#NULL!'
  CONST                    ::= NUMBER | STRING | BOOL
  NUMBER                   ::= #'[0-9]+\\.?[0-9]*(e[0-9]+)?'
  STRING                   ::= #'\"[^\"]*\"' | #\"'[^']*'\"
  BOOL                     ::= #'TRUE|FALSE|True|False|true|false'
  FNCALL                   ::= FN <opening-parenthesis> ARGUMENTS <closing-parenthesis>
  FN                       ::= #'[A-Z]+'
  ARGUMENTS                ::= ARGUMENT {<comma> ARGUMENT}
  ARGUMENT                 ::= EXPR | Epsilon
  OBJREF                   ::= FIELD (( <dot> FIELD ) | ( <dot>? <opening-square-bracket> ( OBJREF | NUMBER_FIELD | FNCALL | STAR  ) <closing-square-bracket> ) )*
  FIELD                    ::= STRING_FIELD | SYMBOL_FIELD | QUOTED_SYMBOL_FIELD | FNCALL | DYNAMIC_REF
  STRING_FIELD             ::= STRING
  SYMBOL_FIELD             ::= #'[^ .,\"\\'\\[\\]\\(\\)\\+\\*/<=>\\^&%]+'
  QUOTED_SYMBOL_FIELD      ::= #\"#'[^']*'\"
  NUMBER_FIELD             ::= #'[0-9]+'
  DYNAMIC_REF              ::= '_'
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
  ")

(def parser (insta/parser parser-str))

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
        :EXP_EXPR
        :STRING
        :DYNAMIC_REF]))

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
   :QUOTED_SYMBOL_FIELD (fn [s]
                          (second (string/split s #"'")))
   :STRING              (fn [s]
                          (let [s (apply str (-> s rest butlast))]
                            (if (reserved? s)
                              (throw (#?(:clj Exception.
                                         :cljs js/Error.) (str "String " s " is reserved.")))
                              [:STRING s])))
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
                              (keyword? (first operand)) (vec (cons :SIGN_EXPR args)))))
   :PERCENT_EXPR        (fn [arg]
                          (if (number? arg)
                            (float (/ arg 100))
                            [:PERCENT_EXPR arg]))
   :MORE_OR_EQ_EXPR     (optimize-token :MORE_OR_EQ_EXPR)
   :ARRAY_EXPR          (fn [& args]
                          (vec (cons :VECTOR args)))
   :ERROR               error/error})

(defn compile* [formula-str & custom-transforms]
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

(defn- replace-dynamic-ref-with-value
  ([expr value]
   (if (or (= expr [:DYNAMIC_REF "_"])
           (= expr ["DYNAMIC_REF" "_"]))
     value
     (cond
       (string? expr)
       expr

       (and (coll? expr)
            (= "MAP" (second expr)))
       [:FNCALL "MAP" [(-> expr (nth 2) first)
                       (replace-dynamic-ref-with-value
                        (-> expr (nth 2) second)
                        value)]]

       (coll? expr)
       (mapv (fn [x]
               (replace-dynamic-ref-with-value x value))
             expr)

       :otherwise
       expr))))

(declare run*)

(defn objref-function [args context]
  (let [first-arg (first args)
        maybe-context (if (vector? first-arg)
                        (run* first-arg context)
                        first-arg)
        rest-args (map #(run* % context) (rest args))
        context (if ((some-fn string? keyword?) maybe-context)
                  context
                  maybe-context)
        args (if ((some-fn string? keyword?) maybe-context)
               (cons maybe-context rest-args)
               rest-args)]
    (with-indifferent-access context args)))

(defn- make-fn [expr context]
  (fn [el]
    (run* (replace-dynamic-ref-with-value expr el) context)))

(def-excel-fn objref
  {:special-form true}
  [args context]
  (objref-function args context))

(def-excel-fn if
  "Returns one value if a logical expression is TRUE and another if it is FALSE."
  {:special-form true
   :args [{:desc "An expression or reference to a context value containing an expression that represents some logical value, i.e. TRUE or FALSE."}
          {:desc "The value the function returns if arg1 is TRUE."}
          {:desc "The value the function returns if arg1 is FALSE."
           :opt true}]}
  [args context]
  (if (run* (first args) context)
    (run* (second args) context)
    (when-let [else-expr (nth args 2 nil)]
      (run* else-expr context))))

(def-excel-fn map
  "Returns a result of applying first argument as a anonumous function for each element in second argument."
  {:special-form true
   :args [{:desc "Anonumous function. Any valid formula with a dynamic references (_) as a placeholder for the value."}
          {:desc "An expression or reference to a context value containing an expression that represents an array."}]}
  [args context]
  (mapv (make-fn (first args) context)
        (run* (second args) context)))

(def-excel-fn filter
  "Returns a filtered version of the source array."
  {:special-form true
   :args [{:desc "The data to be filtered."}
          {:desc "Anonumous function. Any valid formula with a dynamic references (_) as a placeholder for the examined value."}]}
  [args context]
  (filter (make-fn (second args) context)
          (run* (first args) context)))

(def-excel-fn sort
  "Sorts the rows of a given array or range by the result of function call."
  {:special-form true
   :args [{:desc "The data to be sorted."}
          {:desc "Anonumous function. Any valid formula with a dynamic references (_) as a placeholder for the examined value."}]}
  [args context]
  (sort-by (make-fn (second args) context)
           (run* (first args) context)))

(def-excel-fn unique
  "Returns unique values in the provided source range, discarding duplicates. Values are returned in the order in which they first appear in the source range."
  {:special-form true
   :args [{:desc "The data to filter by unique entries."}]}
  [args context]
  (distinct (run* (first args) context)))

(defn- run-fncall* [f args context]
  (if-let [fn-impl (find-impl f)]
    (try
      (if (:special-form (meta fn-impl))
        (fn-impl args context)
        (let [args (mapv #(run* % context) args)]
          (if (empty? args)
            (fn-impl)
            (apply fn-impl args))))
      (catch #?(:clj clojure.lang.ExceptionInfo
                :cljs ExceptionInfo) e
        (throw e))
      (catch #?(:clj clojure.lang.ArityException
                :cljs js/Error) e
        (let [{:keys [name]} (meta fn-impl)]
          (throw (error/error "#N/A"
                              (str "Wrong number of args (" (count args) ") passed to: " name))))))))

(defmulti ->keyword (fn [v] (type v)))

(defmethod ->keyword #?(:clj String
                        :cljs js/String) [v] (keyword v))

(defmethod ->keyword #?(:clj clojure.lang.Keyword
                        :cljs cljs.core/Keyword) [v] v)

(defmulti ->string (fn [v] (type v)))

(defmethod ->string #?(:clj String
                       :cljs js/String) [v] v)

(defmethod ->string #?(:clj clojure.lang.Keyword
                       :cljs cljs.core/Keyword) [v] (name v))

(defn- excel-error? [maybe-error]
  (if-let [{error-type :type} (ex-data maybe-error)]
    (contains? #{"#N/A" "#VALUE!" "#REF!" "#DIV/0!" "#NUM!" "#NAME?" "#NULL!"} error-type)))

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
                           (if (number? r)
                             (if (= (first args) "-")
                               (* -1 r)
                               r)
                             (throw (error/error "#VALUE!" (error/format-not-a-number-error "SIGN" 1 r)))))
        :EXP_EXPR        (let [f (run* (first args) context)
                               s (run* (second args) context)
                               res (Math/pow f s)]
                           (if (and (integer? f) (integer? s))
                             (int res)
                             res))
        :PERCENT_EXPR    (let [r (run* (first args) context)]
                           (float (/ r 100)))
        :OBJREF          (objref-function args context)
        :DYNAMIC_REF     context
        :VECTOR          (mapv #(run* % context) args)
        :FNCALL          (run-fncall* (first args) (second args) context)
        :STRING          (first args))

      (and token args)
      arg

      (or (string? token)
          (number? token)
          (boolean? token)
          (keyword? token)
          (excel-error? token))
      token)))

(defn compile [formula-str & custom-transforms]
  (try
    (apply compile* formula-str custom-transforms)
    (catch #?(:clj Throwable
              :cljs js/Error) e
      (throw (error/error "#ERROR!" "Formula parse error." (ex-data e))))))

(defn run
  ([formula] (run formula {}))
  ([formula context]
   (let [formula-or-error (if (string? formula)
                            (compile formula)
                            formula)]
     (try
       (run* formula-or-error context)
       (catch #?(:clj Throwable
                 :cljs js/Error) e
         (if (excel-error? e)
           (ex-data e)
           (throw e)))))))
