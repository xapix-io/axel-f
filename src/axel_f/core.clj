(ns axel-f.core
  (:require [instaparse.core :as insta]
            [clojure.string :as string]))

(defn- set->regex [string-set]
  (str "#'("
       (string/join "|" string-set)
       ")+'"))

(defn- set->rule [string-set]
  (->> string-set
     (map #(str "'" % "'"))
     (string/join " | ")))

(def whitespace-symbols
  #{"\\s"})

(def functions #{"IF" "SUM" "MIN"})

(def grammar
  (str
   "
   EXPR = <paren_op> EXPR <paren_close> | FNCALL | ADD_EXPR | number | OBJREF
   ADD_EXPR = MULT_EXPR | ADD_EXPR <whitespace>* ('+' | '-' ) <whitespace>* MULT_EXPR
   MULT_EXPR = number | MULT_EXPR <whitespace>* ('*' | '/') <whitespace>* EXPR
   FNCALL = FNNAME <paren_op> ARGLIST <paren_close>
   FNNAME = " (set->rule functions) "
   ARGLIST = ARG | ARG <whitespace>* <comma> <whitespace>* ARGLIST
   <ARG> = EXPR
   OBJREF = REF
   REF = OBJ | REF <dot> FIELD
   OBJ = identifier
   FIELD = identifier
   <if> = 'if'
   <comma> = ','
   <dot> = '.'
   <paren_op> = '('
   <paren_close> = ')'
   <whitespace> = " (set->regex whitespace-symbols) "
   identifier = #'[a-zA-Z_]+[a-zA-Z0-9_]*'
   number = #'-?[0-9]+'

  "))


(def parser
  (insta/parser grammar))

;; (clojure.pprint/pprint (parser "SUM(foo.baz.bar - MIN(1,2) + 11)"))

;; (parser "obj.")

(def ^:dynamic *object-context* {})

(defn operation [str-op & args]
  (let [op (-> str-op symbol resolve)]
    (apply op args)))

(defmulti run #(first %))

(defmethod run :EXPR EXPR [nodes]
  (run (second nodes)))

(defmethod run :number number [[_ number-str]]
  (Integer/parseInt number-str))

(defn math-expr [child-nodes]
  (if (= (count child-nodes) 3)
    (let [[expr1 str-op expr2] child-nodes]
      (operation
       str-op (run expr1) (run expr2)))
    (run (first child-nodes))))

(defmethod run :ADD_EXPR ADD_EXPR [[_ & child-nodes]]
  (math-expr child-nodes))

(defmethod run :MULT_EXPR MULT_EXPR [[_ & child-nodes]]
  (math-expr child-nodes))

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

(defmethod run :OBJREF OBJREF [[node-name & arglist]]
  (get-in *object-context*
          (run (first arglist))))

(defmethod run :REF REF [[node-name & arglist]]
  (concat (run (first arglist))
          (when-let [tail (-> arglist second second second)]
            [tail])))

(defmethod run :FIELD FIELD [[node-name & arglist]]
  (-> arglist second second))

(defmethod run :OBJ OBJ [[node-name & arglist]]
  [(second (first arglist))])


(comment
  (parser "abc.foo.bar.baz")
  (parser "foo(1 + 1)")
  (parser "1")
  (run (parser "-1"))
  (run (parser "1 + 2 - 3 + 4"))
  (parser "1 + 2 - 3 + 4")

  (binding [*object-context* {"abc" {"foo" {"bar" {"baz" 42}}}}]
    (run
      (parser "abc.foo.bar.baz * 2")
      ))

  (binding [*object-context* {"foo" {"baz" {"bar" 42}}}]
    (run
      (parser "SUM(foo.baz.bar, 8, 10) - MIN(1,2) + 11")

      ))

  (run (parser "2*3 + 2 - 3 * 4 - 1*2"))

  (clojure.pprint/pprint
   (parser "1 + 2 - 3 + 4"))


  (clojure.pprint/pprint
  (parser "MIN(1,2)"))

  (run (parser "MIN(1,2,2,3,4,5,11,1,2,3,4)"))

  (run (parser "SUM(10 - MIN(3,2) * 11)"))

  (run (parser "SUM(1 + 1)"))

  (parser "SUM(-(1 + 1)")

  (clojure.pprint/pprint
   (parser "1 + 1"))

  (run
    (parser "1+1-2"))

  (run
    (parser "11 > 2"))

  )
