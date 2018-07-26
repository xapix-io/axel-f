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

(defmethod run :EXPR [nodes]
  (run (second nodes)))

(defmethod run :number [[node-name number-str]]
  (Integer/parseInt number-str))

(defmethod run :MULT_EXPR [[node-name & child-nodes]]
  (if (= (count child-nodes) 3)
    (let [[expr1 [_ str-op] expr2] child-nodes]
      (operation
       str-op (run expr1) (run expr2)))
    (run (first child-nodes))))

(comment
  (parser "foo.bar")
  (parser "foo(1 + 1)")
  (parser "1 + 1 - 1")
  (parser "SUM(foo.baz.bar - min(1,2) + 11)")

  (clojure.pprint/pprint
   (parser "1 + 1"))

  (run
    (parser "1+1-2"))

  (run
    (parser "11 > 2"))

  )
