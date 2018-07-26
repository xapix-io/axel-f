(ns axel-f.core
  (:require [instaparse.core :as insta]
            [clojure.string :as str]))

(defn- set->regex [string-set]
  (str "#'("
       (str/join "|" string-set)
       ")+'"))

(defn- set->rule [string-set]
  (->> string-set
     (map #(str "'" % "'"))
     (str/join " | ")))

(def whitespace-symbols
  #{"\\s"})

(def operators #{"+" "-" "/" "*"
                 "=" "!=" "<" ">" "<=" ">="})

(def functions #{"IF" "SUM" "MIN"})

(def grammar
  (str
   "
   EXPR = <paren_op> EXPR <paren_close> | FNCALL | MULT_EXPR | number |  REF
   MULT_EXPR = EXPR | MULT_EXPR <whitespace>* OPERATOR <whitespace>* EXPR
   FNCALL = FNNAME <paren_op> ARGLIST <paren_close>
   FNNAME = " (set->rule functions) "
   ARGLIST = ARG | ARG <whitespace>* <comma> <whitespace>* ARGLIST
   <ARG> = EXPR
   OPERATOR = " (set->rule operators) "
   REF = OBJ <dot> FIELD | REF <dot> FIELD
   OBJ = wordnumber
   FIELD = wordnumber
   <if> = 'if'
   <comma> = ','
   <dot> = '.'
   <paren_op> = '('
   <paren_close> = ')'
   <whitespace> = " (set->regex whitespace-symbols) "
   wordnumber = #'[a-zA-Z0-9_]+'
   number = #'[0-9]+'

  "))


(def parser
  (insta/parser grammar))

;; (clojure.pprint/pprint (parser "SUM(foo.baz.bar - MIN(1,2) + 11)"))

;; (parser "obj.")

(defn operation [str-op & args]
  (let [op (-> str-op symbol resolve)]
    (apply op args)))

(defmulti run #(first %))

(defmethod run :EXPR EXPR [nodes]
  (run (second nodes)))

(defmethod run :number number [[_ number-str]]
  (Integer/parseInt number-str))

(defmethod run :MULT_EXPR MULT_EXPR [[_ & child-nodes]]
  (if (= (count child-nodes) 3)
    (let [[expr1 [_ str-op] expr2] child-nodes]
      (operation
       str-op (run expr1) (run expr2)))
    (run (first child-nodes))))

(defmulti fncall #(first %))

(defmethod fncall :MIN MIN [[_ arglist]]
  (apply min arglist))

(defmethod fncall :SUM SUM [[_ arglist]]
  (apply + arglist))

(defmethod run :FNCALL FNCALL [[_ & [[_ fn-name] arglist-node]]]
  (fncall  [(keyword fn-name)
            (flatten (run arglist-node))]))

(defmethod run :ARGLIST ARGLIST [[node-name & arglist]]
  (map run arglist))

(comment
  (parser "foo.bar")
  (parser "foo(1 + 1)")
  (parser "1 + 1 - 1")
  (clojure.pprint/pprint
   (parser "SUM(foo.baz.bar - MIN(1,2) + 11)"))

  (clojure.pprint/pprint
   (parser "MIN(1,2)"))

  (run (parser "MIN(1,2,2,3,4,5,11,1,2,3,4)"))

  (run (parser "SUM(0 - MIN(1,2) + 11)"))

  (clojure.pprint/pprint
   (parser "1 + 1"))

  (run
    (parser "1+1-2"))

  (run
    (parser "11 > 2"))

  )
