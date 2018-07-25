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
   EXPR = '(' EXPR ')' | FNCALL | NUMBER | EXPR <whitespace> OPERATOR <whitespace> EXPR | REF
   FNCALL = FNNAME '(' ARGLIST ')'
   FNNAME = WORDNUMBER
   ARGLIST = EXPR | EXPR ',' ARGLIST
   OPERATOR = " (set->rule operators) "
   REF = OBJ '.' FIELD | REF '.' FIELD
   OBJ = WORDNUMBER
   FIELD = WORDNUMBER
   <whitespace> = " (set->regex whitespace-symbols) "
   WORDNUMBER = #'[a-zA-Z0-9_]+'
   NUMBER = #'[0-9]+'

  "))
(def parser
  (insta/parser grammar :output-format :enlive))

(comment
  (parser "foo.bar")
  (parser "foo(1 + 1)")
  (parser "1 + 1")
  (clojure.pprint/pprint (parser "SUM(foo.baz.bar - min(1,2) + 11)"))
  )
