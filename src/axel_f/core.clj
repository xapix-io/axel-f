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
   EXPR = '(' EXPR ')' | FNCALL | <number> | EXPR <whitespace> OPERATOR <whitespace> EXPR | REF
   FNCALL = <wordnumber> '(' ARGLIST ')'
   ARGLIST = EXPR | EXPR ',' ARGLIST
   OPERATOR = " (set->rule operators) "
   REF = <wordnumber> '.' <wordnumber> | REF '.' <wordnumber>
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
  (parser "SUM(foo.baz.bar - 11)")
  )
