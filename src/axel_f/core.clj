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

(def operators #{"+" "-" "/" "*"})

(def grammar
  (str
   "
   EXPR = <paren_op> EXPR <paren_close> | FNCALL | number | EXPR <whitespace> OPERATOR <whitespace> EXPR | REF
   FNCALL = FNNAME <paren_op> ARGLIST <paren_close>
   FNNAME = wordnumber
   ARGLIST = ARG | ARG <comma> ARGLIST
   <ARG> = EXPR
   OPERATOR = " (set->rule operators) "
   REF = OBJ <dot> FIELD | REF <dot> FIELD
   OBJ = wordnumber
   FIELD = wordnumber
   <comma> = ','
   <dot> = '.'
   <paren_op> = '('
   <paren_close> = ')'
   <whitespace> = " (set->regex whitespace-symbols) "
   <wordnumber> = #'[a-zA-Z0-9_]+'
   <number> = #'[0-9]+'

  "))
(def parser
  (insta/parser grammar :output-format :enlive))



(comment
  (parser "foo.bar")
  (parser "foo(1 + 1)")
  (parser "1 + 1")
  (parser "SUM(foo.baz.bar - min(1,2) + 11)")
  )
