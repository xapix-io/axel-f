(ns axel-f.v2.parser-next
  (:require [axel-f.v2.reader :as reader]
            [axel-f.v2.lexer :as lexer]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.walk :as walk]))

(def constant?
  (some-fn lexer/number-literal? lexer/text-literal?))

(defn expression? [{:keys [kind]}]
  (and kind (= (namespace kind) (str *ns*))))

(defn ->function-node [{:keys [value begin] :as f} args]
  {:kind ::fncall
   :f f
   :args args
   :begin begin
   :end (-> args last :end)})

(defn into-reader [ts]
  (-> ts reader/reader reader/push-back-reader))

(declare parse-primary parse-expression)

(defn parse-arguments [ts]
  (let [{:keys [depth] :as args-begin} (reader/read-elem ts)]
    (loop [args []]
      (let [arg-ts (reader/read-until ts #(and (or (lexer/punctuation-literal? % ",")
                                                   (lexer/bracket-literal? % ")"))
                                               (= depth (:depth %))))
            arg-end (reader/read-elem ts)
            ts (into-reader arg-ts)
            args' (conj args (parse-primary ts))]
        (if (lexer/bracket-literal? arg-end ")")
          args'
          (recur args'))))))

(defn parse-prefix-function-call [ts]
  (let [f (reader/read-elem ts)]
    (->function-node f (parse-expression ts))))

(defn parse-function-call [ts]
  (let [f (reader/read-elem ts)]
    (->function-node f (parse-arguments ts))))

(defn parse-keyword [ts]
  (loop [acc [(reader/read-elem ts)] next (reader/peek-elem ts)]
    (cond
      (and (lexer/operator-literal? (last acc) ["/"])
           ((some-fn lexer/symbol-literal? lexer/text-literal?) next))
      {:kind ::lexer/symbol
       :value (keyword (string/join "." (->> acc rest butlast
                                             (remove #(lexer/punctuation-literal? % ["."]))
                                             (map :value)))
                       (:value (reader/read-elem ts)))
       :begin (-> acc first :begin)
       :end (:end next)}

      (or (and ((some-fn lexer/symbol-literal? lexer/text-literal?) (last acc))
               (or (lexer/punctuation-literal? next ["."])
                   (lexer/operator-literal? next ["/"])))
          (and ((some-fn lexer/symbol-literal? lexer/text-literal?) next)
               (or (lexer/punctuation-literal? (last acc) ["."])
                   (lexer/operator-literal? (last acc) ["/"])))
          (and (lexer/operator-literal? (last acc) [":"])
               (= 1 (count acc))
               ((some-fn lexer/symbol-literal? lexer/text-literal?) next)))
      (recur (conj acc (reader/read-elem ts))
             (reader/peek-elem ts)))))

(defn parse-constant [ts]
  (let [{:keys [value] :as c} (reader/read-elem ts)]
    (assoc c :kind ::constant)))

(defn combine [fel {:keys [value end] :as sel}]
  (-> fel
      (update :value #(conj % (if (expression? sel)
                                sel
                                value)))
      (assoc :end end)))

(defn parse-symbol-expression [rdr]
  (let [current (reader/read-elem rdr)
        {:keys [value] :as next-el} (reader/peek-elem rdr)]
    (if (and (contains? #{"true" "True" "TRUE" "false" "False" "FALSE"} (:value current))
             (not (lexer/punctuation-literal? next-el ["."])))
      {:kind ::constant
       :value (-> current :value string/lower-case edn/read-string)
       :begin (:begin current)
       :end (:end current)}
      (let [current (if (expression? current)
                      current
                      {:kind ::symbol
                       :value [(:value current)]
                       :begin (:begin current)
                       :end (:end current)})]
        (cond
          ;; Skip dots
          (lexer/punctuation-literal? next-el ["."])
          (do (reader/read-elem rdr)
              (let [{:keys [value begin end] :as next-el} (reader/peek-elem rdr)]
                (when-not
                    (or (lexer/symbol-literal? next-el)
                        (lexer/text-literal? next-el)
                        (lexer/bracket-literal? next-el ["["]))
                  (throw (ex-info (str "array reference, string or symbol are expected after dot in reference, got `" value "`")
                                  {:position [begin end]
                                   :token next-el}))))
              (reader/unread-elem rdr current)
              (parse-symbol-expression rdr))

          ;; Reparse as function call
          (lexer/bracket-literal? next-el ["("])
          (do (reader/unread-elem rdr current)
              (parse-function-call rdr))

          (lexer/bracket-literal? next-el ["["])
          (let [{:keys [depth]} (reader/read-elem rdr)
                tokens (reader/read-until rdr #(and (lexer/bracket-literal? % ["]"])
                                                    (= depth (:depth %))))
                {:keys [end]} (reader/read-elem rdr)]
            (if (and (= 1 (count tokens))
                     (= "*" (:value (first tokens))))
              (do (reader/unread-elem rdr (combine current (first tokens)))
                  (parse-symbol-expression rdr))
              (let [rdr' (-> tokens reader/reader reader/push-back-reader)
                    idx (parse-primary rdr')]
                (if (reader/peek-elem rdr')
                  (throw (ex-info "Array reference must have exactly one element"
                                  {:position [(:begin (reader/peek-elem rdr'))
                                              end]}))
                  (do (reader/unread-elem rdr (combine current idx))
                      (parse-symbol-expression rdr))))))

          (or (lexer/symbol-literal? next-el)
              (lexer/text-literal? next-el))
          (do (reader/unread-elem rdr (combine current (reader/read-elem rdr)))
              (parse-symbol-expression rdr))

          ;; Stop parsing symbolyc expression
          :otherwise
          current)))))

(defn symbol-literal? [{:keys [kind]}]
  (= kind ::symbol))

(defn keyword-literal? [{:keys [kind]}]
  (= kind ::keyword))

(defn build-function-name [{:keys [value begin end] :as t}]
  (if (every? string? value)
    (string/join "." value)
    (throw (ex-info "Unknown function"
                    {:position [begin end]
                     :token t}))))

(defn build-reference-args [{:keys [value] :as t}]
  value)

(defn parse-symbol-expression [ts]
  (let [current (reader/read-elem ts)
        n (reader/peek-elem ts)]
    (cond
      (and (symbol-literal? current)
           (lexer/punctuation-literal? n ["."]))
      ;; Read next token and throw if it is not array index or symbol or text
      (let [_ (reader/read-elem ts)
            n (reader/peek-elem ts)]
        (if (or (lexer/symbol-literal? n)
                (lexer/bracket-literal? n ["["]))
          (assoc current :after-dot true)
          (throw (ex-info "Unexpected token in reference expression"
                          {:position ((juxt :begin :end) n)
                           :token n}))))

      (and (symbol-literal? current)
           (lexer/bracket-literal? n ["["]))
      ;; Read array index expression
      (let [{:keys [begin depth] :as ob} (reader/read-elem ts)
            tokens (reader/read-until ts #(and (lexer/bracket-literal? % ["]"])
                                               (= depth (:depth %))))
            {:keys [end] :as cb} (reader/read-elem ts)]
        (-> current
            (update :value #(conj (if (vector? %) % [%])
                                  {:kind ::array-reference
                                   :value (if (and (= 1 (count tokens))
                                                   (lexer/operator-literal? (first tokens) ["*"]))
                                            "*"
                                            (parse-primary (into-reader tokens)))
                                   :begin begin
                                   :end end}))
            (assoc :end end)
            (assoc :kind ::symbol)))

      (and (lexer/symbol-literal? current)
           (contains? #{"TRUE" "FALSE"} (:value current)))
      ;; Return constant
      (if (or (lexer/punctuation-literal? n ["."])
              (lexer/bracket-literal? n ["["]))
        (assoc current :kind ::symbol)
        {:kind ::constant
         :value (case (:value current)
                  "TRUE" true
                  "FALSE" false)
         :begin (:begin current)
         :end (:end current)})

      (and (symbol-literal? current)
           (:after-dot current)
           (lexer/symbol-literal? n))
      (let [{:keys [value end]} (reader/read-elem ts)]
        (-> current
            (update :value conj value)
            (assoc :end end)
            (assoc :after-dot false)))

      (lexer/symbol-literal? current)
      (-> current
          (update :value vector)
          (assoc :kind ::symbol))

      (lexer/bracket-literal? n ["("])
      {:kind ::fnname
       :value (build-function-name current)
       :begin (:begin current)
       :end (:end current)}

      :otherwise
      {:kind ::fncall
       :f {:kind ::fnname
           :value "get-in"}
       :args (build-reference-args current)
       :begin (:begin current)
       :end (:end current)})))

(defn function-name? [{:keys [kind]}]
  (= ::fnname kind))

(defn parse-many [ts pred]
  (loop [acc [] tokens (reader/read-until ts pred)]
    (if (not-empty tokens)
      (recur (conj acc (parse-primary (into-reader tokens)))
             (do (reader/read-elem ts)
                 (reader/read-until ts pred)))
      acc)))

(defn parse-function-call [ts]
  (let [f (reader/read-elem ts)
        {:keys [begin depth] :as ob} (reader/read-elem ts)
        tokens (reader/read-until ts #(and (lexer/bracket-literal? % [")"])
                                           (= depth (:depth %))))
        {:keys [end] :as cb} (reader/read-elem ts)
        ts' (into-reader tokens)
        div-pred #(or (and (lexer/punctuation-literal? % [","])
                           (= (inc depth) (:depth %)))
                      (nil? %))]
    {:kind ::fncall
     :f f
     :args (parse-many ts' div-pred)}))

(defn begin-array? [t]
  (lexer/bracket-literal? t ["["]))

(defn parse-array [ts]
  (let [{:keys [begin depth] :as ob} (reader/read-elem ts)
        tokens (reader/read-until ts #(and (lexer/bracket-literal? % ["]"])
                                           (= depth (:depth %))))
        {:keys [end] :as cb} (reader/read-elem ts)
        ts' (into-reader tokens)
        div-pred #(or (and (lexer/punctuation-literal? % [","])
                           (= (inc depth) (:depth %)))
                      (nil? %))]
    {:kind ::fncall
     :f {:kind ::fnname
         :value "vec"}
     :args (parse-many ts' div-pred)}))

(defn begin-group? [t]
  (lexer/bracket-literal? t ["("]))

(defn parse-group [ts]
  (let [{:keys [begin depth] :as ob} (reader/read-elem ts)
        tokens (reader/read-until ts #(and (lexer/bracket-literal? % [")"])
                                           (= depth (:depth %))))
        {:keys [end] :as cb} (reader/read-elem ts)
        ts' (into-reader tokens)]
    (parse-primary ts')))

(defn parse-postfix [operator arg]
  {:kind ::fncall
   :f (assoc operator :kind ::fnname)
   :args [arg]
   :begin (:begin arg)
   :end (:end operator)})

(defn fncall? [{:keys [kind]}]
  (= ::fncall kind))

(defn const->fncall [{:keys [value begin end]}]
  {:kind ::fncall
   :f {:kind ::fnname
       :value "const"}
   :args [value]
   :begin begin
   :end end})

;; Operator precedence

(defn- -op? [v]
  (fn [op]
    (lexer/operator-literal? op [v])))

(def ^:private concat-op? (-op? "&"))

(def ^:private eq-op? (-op? "="))

(def ^:private not-eq-op? (-op? "<>"))

(def ^:private less-or-eq-op? (-op? "<="))

(def ^:private more-or-eq-op? (-op? ">="))

(def ^:private less-op? (-op? "<"))

(def ^:private more-op? (-op? ">"))

(def ^:private sub-op? (-op? "-"))

(def ^:private add-op? (-op? "+"))

(def ^:private div-op? (-op? "/"))

(def ^:private mult-op? (-op? "*"))

(def ^:private exp-op? (-op? "^"))

(def ^:private next-op-pred
  {concat-op? eq-op?
   eq-op? not-eq-op?
   not-eq-op? less-or-eq-op?
   less-or-eq-op? more-or-eq-op?
   more-or-eq-op? less-op?
   less-op? more-op?
   more-op? sub-op?
   sub-op? add-op?
   add-op? div-op?
   div-op? mult-op?
   mult-op? exp-op?})

(defn- infix->fncall
  ([infix-args] (infix->fncall infix-args concat-op?))
  ([infix-args op-pred]
   (if (and op-pred (not= 1 (count infix-args)))
     (let [infix-args' (partition-by op-pred infix-args)]
       (if (= 1 (count infix-args'))
         (if-let [op-pred' (next-op-pred op-pred)]
           (infix->fncall infix-args op-pred')
           (first infix-args'))
         (let [f (-> infix-args' second first)]
           {:kind ::fncall
            :f (assoc f :kind ::fnname)
            :args (map #(infix->fncall (first %) (next-op-pred op-pred))
                       (partition 1 2 infix-args'))})))
     (first infix-args))))

(defn parse-expression [ts]
  (let [{:keys [value] :as next-token} (reader/peek-elem ts)]
    (cond
      (constant? next-token)
      (let [c (parse-constant ts)
            n (reader/peek-elem ts)]
        (if (lexer/operator-literal? n ["%"])
          c
          (const->fncall c)))

      (lexer/operator-literal? next-token ["+" "-" "!"])
      (parse-prefix-function-call ts)

      (lexer/operator-literal? next-token [":"])
      (parse-keyword ts)

      (function-name? next-token)
      (parse-function-call ts)

      (begin-group? next-token)
      (parse-group ts)

      (begin-array? next-token)
      (parse-array ts)

      (or (lexer/symbol-literal? next-token)
          (symbol-literal? next-token))
      (parse-symbol-expression ts)

      (expression? next-token)
      (let [next-token (reader/read-elem ts)
            next-token' (reader/peek-elem ts)]
        (cond
          (lexer/operator-literal? next-token' ["%"])
          (parse-postfix (reader/read-elem ts) next-token)

          (lexer/operator-literal? next-token' ["+" "-" "*" "/" "&" "<" ">" "<=" ">=" "<>" "^"])
          (loop [acc [next-token (reader/read-elem ts)]]
            (let [expr (parse-primary ts true)
                  next-op (reader/peek-elem ts)]
              (if (not-empty expr)
                (if (lexer/operator-literal? next-op ["+" "-" "*" "/" "&" "<" ">" "<=" ">=" "<>" "^"])
                  (recur (conj acc expr (reader/read-elem ts)))
                  ;;
                  (infix->fncall (conj acc expr)))
                (infix->fncall acc))))

          :otherwise
          (throw (ex-info (str "Expression must be followed by postfix or infix operator, got `" (:value next-token') "` instead.")
                          {:position ((juxt :begin :end) next-token')
                           :token next-token'}))))

      :otherwise
      (throw (ex-info "Unexpected token"
                      {:position ((juxt :begin :end) next-token)
                       :token next-token})))))

(defn const? [{:keys [kind]}]
  (= ::constant kind))

(defn parse-primary
  ([tokens-rdr] (parse-primary tokens-rdr false))
  ([tokens-rdr stop-on-complete-expr?]
   (let [expr (parse-expression tokens-rdr)]
     ;; (if (reader/peek-elem tokens-rdr)
     ;;   (if (and (fncall? expr)
     ;;            stop-on-complete-expr?)
     ;;     expr
     ;;     (do (reader/unread-elem tokens-rdr expr)
     ;;         (parse-primary tokens-rdr stop-on-complete-expr?)))
     ;;   (if (or (symbol-literal? expr)
     ;;           (lexer/symbol-literal? expr))
     ;;     (do (reader/unread-elem tokens-rdr expr)
     ;;         (parse-primary tokens-rdr stop-on-complete-expr?))
     ;;     expr))

     (cond
       ;; Some tokens left in the reader and parser is not forced to stop after first parsed fncall
       (and (reader/peek-elem tokens-rdr)
            (not (and stop-on-complete-expr? (fncall? expr))))
       (do (reader/unread-elem tokens-rdr expr)
           (parse-primary tokens-rdr stop-on-complete-expr?))

       ;; Continue parsing when no tokens to parse but expression not in form of fncall
       (and (empty? (reader/peek-elem tokens-rdr))
            (or (symbol-literal? expr)
                (lexer/symbol-literal? expr)))
       (do (reader/unread-elem tokens-rdr expr)
           (parse-primary tokens-rdr stop-on-complete-expr?))

       :otherwise
       expr))))

(def resolve-function
  {"+" +'
   "-" -'
   "*" *'
   "/" /
   "&" str
   "<" <
   ">" >
   "<=" <=
   ">=" >=
   "<>" not=
   "!" not
   "get-in" #(get-in %1 %2)})

(defn- select-ctx [xs]
  (if-let [[_ position] (re-matches #"_([1-9][0-9]*)" (first xs))]
    [(list 'nth 'args (dec (Integer/parseInt position)) nil) (rest xs)]
    (if (= "_" (first xs))
      [(list 'nth 'args 0) (rest xs)]
      [(list 'ctx) xs])))

(defmulti emit-node* (fn [fnname _] fnname))

(defmethod emit-node* "const" [_ [c]] c)

(defmethod emit-node* "vec" [_ args] args)

(defmethod emit-node* "MAP" [_ [f & cs]]
  (let [f' (list 'fn '[& args] f)]
    (concat (list 'map f')
            cs)))

(defmethod emit-node* "get-in" [_ args]
  (let [[ctx path] (select-ctx args)]
    (concat (list (resolve-function "get-in"))
            (list ctx)
            (list path))))

(defmethod emit-node* :default [fnname args]
  (let [f (resolve-function fnname)]
    (cons f args)))

(defn emit-node [{{fnname :value} :f
                  args :args}]
  (emit-node* fnname args))


;; (parse "1 + 1 * :fo.bo/ba.foo.bar")
;; => (fn [ctx]
;;      (axel-f.functions/+
;;         1
;;         (axel-f.functions/*
;;            1
;;            (axel-f.functions/get-in*
;;               ctx
;;               [:fo.bo/ba "foo" "bar"]))))
(defn parse [formula]
  (let [ast (-> formula lexer/read-formula
                reader/reader reader/push-back-reader
                parse-primary)]
    (list 'fn '[ctx]
          (walk/postwalk
           (fn [node]
             (if (and (map? node) (fncall? node))
               (emit-node node)
               node))
           ast))))

(comment

  (lexer/read-formula "FOO(1,2,3)")

  (walk/postwalk
   (fn [x] (if (and (map? x)
                    (fncall? x))
             (do (prn x)
                 x)
             x))
   (parse "2 + 2 * JSON.DECODE('{\\'x\\':1}')")
   )

  ((eval (parse "MAP(_ + 3, MAP(1 + _1 * _2, [4,2,3], [4,5,6]))")
         ) {})
  )
